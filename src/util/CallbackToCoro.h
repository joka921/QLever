//
// Created by kalmbacj on 10/10/24.
//

#ifndef QLEVER_CALLBACKTOCORO_H
#define QLEVER_CALLBACKTOCORO_H
#include <coroutine>
#include <exception>
#include <functional>
#include <iterator>
#include <type_traits>
#include <utility>

#include "util/Exception.h"
#include "util/TypeTraits.h"

namespace callbackToCoro {
template <typename T>
class generator;

struct GetHandle {};

namespace detail {
template <typename T>
class generator_promise {
 public:
  // Even if the generator only yields `const` values, the `value_type`
  // shouldn't be `const` because otherwise several static checks when
  // interacting with the STL fail.
  using value_type = std::remove_cvref_t<T>;
  using reference_type = std::conditional_t<std::is_reference_v<T>, T, T&>;
  using pointer_type = std::remove_reference_t<T>*;

  generator_promise() = default;

  generator<T> get_return_object() noexcept;

  constexpr std::suspend_always initial_suspend() const noexcept { return {}; }
  constexpr std::suspend_always final_suspend() const noexcept { return {}; }

  template <typename U = T,
            std::enable_if_t<!std::is_rvalue_reference<U>::value, int> = 0>
  std::suspend_always yield_value(std::remove_reference_t<T>& value) noexcept {
    m_value = std::addressof(value);
    return {};
  }

  std::suspend_always yield_value(std::remove_reference_t<T>&& value) noexcept {
    m_value = std::addressof(value);
    return {};
  }

  void unhandled_exception() { m_exception = std::current_exception(); }

  void return_void() {}

  reference_type value() const noexcept {
    return static_cast<reference_type>(*m_value);
  }

  // Don't allow any use of 'co_await' inside the generator coroutine.
  template <typename U>
  std::suspend_never await_transform(U&& value) = delete;

  void rethrow_if_exception() const {
    if (m_exception) {
      std::rethrow_exception(m_exception);
    }
  }

  // The machinery to expose the stored `Details` via
  // `co_await cppcoro::getDetails`.
  struct CallbackAwaiter {
    generator_promise& promise_;
    bool await_ready() const { return true; }
    bool await_suspend(std::coroutine_handle<>) const noexcept { return false; }
    std::coroutine_handle<> await_resume() noexcept {
      return std::coroutine_handle<generator_promise>::from_promise(promise_);
    }
  };

  struct ContinueInnerAwaiter {
    bool await_ready() const { return false; }
    void await_suspend(std::coroutine_handle<>) const noexcept { }
    void await_resume() noexcept {}
  };

  CallbackAwaiter await_transform(
      [[maybe_unused]] ad_utility::SimilarTo<GetHandle> auto&& detail) {
    return {*this};
  }

  ContinueInnerAwaiter await_transform(
      [[maybe_unused]] ad_utility::SimilarTo<ContinueInnerAwaiter> auto&& detail) {
    return {};
  }

 private:
  pointer_type m_value;
  std::exception_ptr m_exception;

};

struct generator_sentinel {};

template <typename T, typename Details>
class generator_iterator {
  using promise_type = generator_promise<T, Details>;
  using coroutine_handle = std::coroutine_handle<promise_type>;

 public:
  using iterator_category = std::input_iterator_tag;
  // What type should we use for counting elements of a potentially infinite
  // sequence?
  using difference_type = std::ptrdiff_t;
  using value_type = typename promise_type::value_type;
  using reference = typename promise_type::reference_type;
  using pointer = typename promise_type::pointer_type;

  // Iterator needs to be default-constructible to satisfy the Range concept.
  generator_iterator() noexcept : m_coroutine(nullptr) {}

  explicit generator_iterator(coroutine_handle coroutine) noexcept
      : m_coroutine(coroutine) {}

  friend bool operator==(const generator_iterator& it,
                         generator_sentinel) noexcept {
    return !it.m_coroutine || it.m_coroutine.done();
  }

  friend bool operator!=(const generator_iterator& it,
                         generator_sentinel s) noexcept {
    return !(it == s);
  }

  friend bool operator==(generator_sentinel s,
                         const generator_iterator& it) noexcept {
    return (it == s);
  }

  friend bool operator!=(generator_sentinel s,
                         const generator_iterator& it) noexcept {
    return it != s;
  }

  generator_iterator& operator++() {
    m_coroutine.resume();
    if (m_coroutine.done()) {
      m_coroutine.promise().rethrow_if_exception();
    }

    return *this;
  }

  // Need to provide post-increment operator to implement the 'Range' concept.
  void operator++(int) { (void)operator++(); }

  reference operator*() const noexcept { return m_coroutine.promise().value(); }

  pointer operator->() const noexcept { return std::addressof(operator*()); }

 private:
  coroutine_handle m_coroutine;
};
}  // namespace detail

template <typename T, typename Details>
class [[nodiscard]] generator {
 public:
  using promise_type = detail::generator_promise<T, Details>;
  using iterator = detail::generator_iterator<T, Details>;
  using value_type = typename iterator::value_type;

  generator() noexcept : m_coroutine(nullptr) {}

  generator(generator&& other) noexcept : m_coroutine(other.m_coroutine) {
    other.m_coroutine = nullptr;
  }

  generator(const generator& other) = delete;

  ~generator() {
    if (m_coroutine) {
      m_coroutine.destroy();
    }
  }

  generator& operator=(generator other) noexcept {
    swap(other);
    return *this;
  }

  iterator begin() {
    if (m_coroutine) {
      m_coroutine.resume();
      if (m_coroutine.done()) {
        m_coroutine.promise().rethrow_if_exception();
      }
    }

    return iterator{m_coroutine};
  }

  detail::generator_sentinel end() noexcept {
    return detail::generator_sentinel{};
  }

  void swap(generator& other) noexcept {
    std::swap(m_coroutine, other.m_coroutine);
  }

  const Details& details() const {
    return m_coroutine ? m_coroutine.promise().details()
                       : m_details_if_default_constructed;
  }
  Details& details() {
    return m_coroutine ? m_coroutine.promise().details()
                       : m_details_if_default_constructed;
  }

  void setDetailsPointer(Details* pointer) {
    m_coroutine.promise().setDetailsPointer(pointer);
  }

 private:
  friend class detail::generator_promise<T, Details>;

  // In the case of an empty, default-constructed `generator` object we still
  // want the call to `details` to return a valid object that in this case is
  // owned directly by the generator itself.
  [[no_unique_address]] Details m_details_if_default_constructed;

  explicit generator(std::coroutine_handle<promise_type> coroutine) noexcept
      : m_coroutine(coroutine) {}

  std::coroutine_handle<promise_type> m_coroutine;
};

template <typename T>
void swap(generator<T>& a, generator<T>& b) {
  a.swap(b);
}

namespace detail {
template <typename T>
generator<T> generator_promise<T>::get_return_object() noexcept {
  using coroutine_handle = std::coroutine_handle<generator_promise<T>>;
  return generator<T>{coroutine_handle::from_promise(*this)};
}
}  // namespace detail

template <typename FUNC, typename T>
generator<
    std::invoke_result_t<FUNC&, typename generator<T>::iterator::reference>>
fmap(FUNC func, generator<T> source) {
  for (auto&& value : source) {
    co_yield std::invoke(func, static_cast<decltype(value)>(value));
  }
}

// Get the first element of a generator and verify that it's the only one.
template <typename T, typename Details>
T getSingleElement(generator<T, Details> g) {
  auto it = g.begin();
  AD_CORRECTNESS_CHECK(it != g.end());
  T t = std::move(*it);
  AD_CORRECTNESS_CHECK(++it == g.end());
  return t;
}
}  // namespace callbackToCoro

#endif  // QLEVER_CALLBACKTOCORO_H
