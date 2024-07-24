//
// Created by kalmbacj on 7/23/24.
//

#include <ranges>

#pragma once

namespace ad_utility::ranges {
namespace r = std::ranges;
using namespace std;

namespace detail {

template <typename _Range>
concept simple_view =
    r::view<_Range> && r::range<const _Range> &&
    same_as<r::iterator_t<_Range>, r::iterator_t<const _Range>> &&
    same_as<r::sentinel_t<_Range>, r::sentinel_t<const _Range>>;

template <typename _Range>
concept range_with_movable_reference =
    r::input_range<_Range> &&
    std::move_constructible<r::range_reference_t<_Range>> &&
    std::move_constructible<r::range_rvalue_reference_t<_Range>>;
}  // namespace detail
// Alias for a type that is conditionally const.
template <bool _Const, typename _Tp>
using maybe_const_t = std::conditional_t<_Const, const _Tp, _Tp>;
namespace views::__adaptor {
// True if the range adaptor _Adaptor can be applied with _Args.
template <typename _Adaptor, typename... _Args>
concept __adaptor_invocable =
    requires { std::declval<_Adaptor>()(declval<_Args>()...); };

// True if the range adaptor non-closure _Adaptor can be partially applied
// with _Args.
template <typename _Adaptor, typename... _Args>
concept __adaptor_partial_app_viable =
    (_Adaptor::_S_arity > 1) && (sizeof...(_Args) == _Adaptor::_S_arity - 1) &&
    (constructible_from<decay_t<_Args>, _Args> && ...);

template <typename _Adaptor, typename... _Args>
struct _Partial;

template <typename _Lhs, typename _Rhs>
struct _Pipe;

// The base class of every range adaptor closure.
//
// The derived class should define the optional static data member
// _S_has_simple_call_op to true if the behavior of this adaptor is
// independent of the constness/value category of the adaptor object.
struct _RangeAdaptorClosure {
  // range | adaptor is equivalent to adaptor(range).
  template <typename _Self, typename _Range>
  requires derived_from<remove_cvref_t<_Self>, _RangeAdaptorClosure> &&
           __adaptor_invocable<_Self, _Range>
  friend constexpr auto operator|(_Range&& __r, _Self&& __self) {
    return std::forward<_Self>(__self)(std::forward<_Range>(__r));
  }

  // Compose the adaptors __lhs and __rhs into a pipeline, returning
  // another range adaptor closure object.
  template <typename _Lhs, typename _Rhs>
  requires derived_from<_Lhs, _RangeAdaptorClosure> &&
           derived_from<_Rhs, _RangeAdaptorClosure>
  friend constexpr auto operator|(_Lhs __lhs, _Rhs __rhs) {
    return _Pipe<_Lhs, _Rhs>{std::move(__lhs), std::move(__rhs)};
  }
};

// The base class of every range adaptor non-closure.
//
// The static data member _Derived::_S_arity must contain the total number of
// arguments that the adaptor takes, and the class _Derived must introduce
// _RangeAdaptor::operator() into the class scope via a using-declaration.
//
// The optional static data member _Derived::_S_has_simple_extra_args should
// be defined to true if the behavior of this adaptor is independent of the
// constness/value category of the extra arguments.  This data member could
// also be defined as a variable template parameterized by the types of the
// extra arguments.
template <typename _Derived>
struct _RangeAdaptor {
  // Partially apply the arguments __args to the range adaptor _Derived,
  // returning a range adaptor closure object.
  template <typename... _Args>
  requires __adaptor_partial_app_viable<_Derived, _Args...>
  constexpr auto operator()(_Args&&... __args) const {
    return _Partial<_Derived, decay_t<_Args>...>{
        std::forward<_Args>(__args)...};
  }
};

// True if the range adaptor closure _Adaptor has a simple operator(), i.e.
// one that's not overloaded according to constness or value category of the
// _Adaptor object.
template <typename _Adaptor>
concept __closure_has_simple_call_op = _Adaptor::_S_has_simple_call_op;

// True if the behavior of the range adaptor non-closure _Adaptor is
// independent of the value category of its extra arguments _Args.
template <typename _Adaptor, typename... _Args>
concept __adaptor_has_simple_extra_args =
    _Adaptor::_S_has_simple_extra_args ||
    _Adaptor::template _S_has_simple_extra_args<_Args...>;

// A range adaptor closure that represents partial application of
// the range adaptor _Adaptor with arguments _Args.
template <typename _Adaptor, typename... _Args>
struct _Partial : _RangeAdaptorClosure {
  tuple<_Args...> _M_args;

  constexpr _Partial(_Args... __args) : _M_args(std::move(__args)...) {}

  // Invoke _Adaptor with arguments __r, _M_args... according to the
  // value category of this _Partial object.
  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, const _Args&...>
  constexpr auto operator()(_Range&& __r) const& {
    auto __forwarder = [&__r](const auto&... __args) {
      return _Adaptor{}(std::forward<_Range>(__r), __args...);
    };
    return std::apply(__forwarder, _M_args);
  }

  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, _Args...>
  constexpr auto operator()(_Range&& __r) && {
    auto __forwarder = [&__r](auto&... __args) {
      return _Adaptor{}(std::forward<_Range>(__r), std::move(__args)...);
    };
    return std::apply(__forwarder, _M_args);
  }

  template <typename _Range>
  constexpr auto operator()(_Range&& __r) const&& = delete;
};

// A lightweight specialization of the above primary template for
// the common case where _Adaptor accepts a single extra argument.
template <typename _Adaptor, typename _Arg>
struct _Partial<_Adaptor, _Arg> : _RangeAdaptorClosure {
  _Arg _M_arg;

  constexpr _Partial(_Arg __arg) : _M_arg(std::move(__arg)) {}

  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, const _Arg&>
  constexpr auto operator()(_Range&& __r) const& {
    return _Adaptor{}(std::forward<_Range>(__r), _M_arg);
  }

  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, _Arg>
  constexpr auto operator()(_Range&& __r) && {
    return _Adaptor{}(std::forward<_Range>(__r), std::move(_M_arg));
  }

  template <typename _Range>
  constexpr auto operator()(_Range&& __r) const&& = delete;
};

// Partial specialization of the primary template for the case where the extra
// arguments of the adaptor can always be safely and efficiently forwarded by
// const reference.  This lets us get away with a single operator() overload,
// which makes overload resolution failure diagnostics more concise.
template <typename _Adaptor, typename... _Args>
requires __adaptor_has_simple_extra_args<_Adaptor, _Args...> &&
         (is_trivially_copyable_v<_Args> && ...)
struct _Partial<_Adaptor, _Args...> : _RangeAdaptorClosure {
  tuple<_Args...> _M_args;

  constexpr _Partial(_Args... __args) : _M_args(std::move(__args)...) {}

  // Invoke _Adaptor with arguments __r, const _M_args&... regardless
  // of the value category of this _Partial object.
  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, const _Args&...>
  constexpr auto operator()(_Range&& __r) const {
    auto __forwarder = [&__r](const auto&... __args) {
      return _Adaptor{}(std::forward<_Range>(__r), __args...);
    };
    return std::apply(__forwarder, _M_args);
  }

  static constexpr bool _S_has_simple_call_op = true;
};

// A lightweight specialization of the above template for the common case
// where _Adaptor accepts a single extra argument.
template <typename _Adaptor, typename _Arg>
requires __adaptor_has_simple_extra_args<_Adaptor, _Arg> &&
         is_trivially_copyable_v<_Arg>
struct _Partial<_Adaptor, _Arg> : _RangeAdaptorClosure {
  _Arg _M_arg;

  constexpr _Partial(_Arg __arg) : _M_arg(std::move(__arg)) {}

  template <typename _Range>
  requires __adaptor_invocable<_Adaptor, _Range, const _Arg&>
  constexpr auto operator()(_Range&& __r) const {
    return _Adaptor{}(std::forward<_Range>(__r), _M_arg);
  }

  static constexpr bool _S_has_simple_call_op = true;
};

template <typename _Lhs, typename _Rhs, typename _Range>
concept __pipe_invocable = requires {
  std::declval<_Rhs>()(std::declval<_Lhs>()(std::declval<_Range>()));
};

// A range adaptor closure that represents composition of the range
// adaptor closures _Lhs and _Rhs.
template <typename _Lhs, typename _Rhs>
struct _Pipe : _RangeAdaptorClosure {
  [[no_unique_address]] _Lhs _M_lhs;
  [[no_unique_address]] _Rhs _M_rhs;

  constexpr _Pipe(_Lhs __lhs, _Rhs __rhs)
      : _M_lhs(std::move(__lhs)), _M_rhs(std::move(__rhs)) {}

  // Invoke _M_rhs(_M_lhs(__r)) according to the value category of this
  // range adaptor closure object.
  template <typename _Range>
  requires __pipe_invocable<const _Lhs&, const _Rhs&, _Range>
  constexpr auto operator()(_Range&& __r) const& {
    return _M_rhs(_M_lhs(std::forward<_Range>(__r)));
  }

  template <typename _Range>
  requires __pipe_invocable<_Lhs, _Rhs, _Range>
  constexpr auto operator()(_Range&& __r) && {
    return std::move(_M_rhs)(std::move(_M_lhs)(std::forward<_Range>(__r)));
  }

  template <typename _Range>
  constexpr auto operator()(_Range&& __r) const&& = delete;
};

// A partial specialization of the above primary template for the case where
// both adaptor operands have a simple operator().  This in turn lets us
// implement composition using a single simple operator(), which makes
// overload resolution failure diagnostics more concise.
template <typename _Lhs, typename _Rhs>
requires __closure_has_simple_call_op<_Lhs> &&
         __closure_has_simple_call_op<_Rhs>
struct _Pipe<_Lhs, _Rhs> : _RangeAdaptorClosure {
  [[no_unique_address]] _Lhs _M_lhs;
  [[no_unique_address]] _Rhs _M_rhs;

  constexpr _Pipe(_Lhs __lhs, _Rhs __rhs)
      : _M_lhs(std::move(__lhs)), _M_rhs(std::move(__rhs)) {}

  template <typename _Range>
  requires __pipe_invocable<const _Lhs&, const _Rhs&, _Range>
  constexpr auto operator()(_Range&& __r) const {
    return _M_rhs(_M_lhs(std::forward<_Range>(__r)));
  }

  static constexpr bool _S_has_simple_call_op = true;
};
}  // namespace views::__adaptor

template <r::view _Vp>
requires detail::range_with_movable_reference<_Vp>
class enumerate_view : public r::view_interface<enumerate_view<_Vp>> {
  _Vp _M_base = _Vp();

  template <bool _Const>
  class _Iterator;
  template <bool _Const>
  class _Sentinel;

 public:
  enumerate_view() requires default_initializable<_Vp> = default;

  constexpr explicit enumerate_view(_Vp base) : _M_base(std::move(base)) {}

  constexpr auto begin() requires(!detail::simple_view<_Vp>) {
    return _Iterator<false>(r::begin(_M_base), 0);
  }

  constexpr auto begin() const
      requires detail::range_with_movable_reference<const _Vp> {
    return _Iterator<true>(r::begin(_M_base), 0);
  }

  constexpr auto end() requires(!detail::simple_view<_Vp>) {
    if constexpr (r::common_range<_Vp> && r::sized_range<_Vp>)
      return _Iterator<false>(r::end(_M_base), r::distance(_M_base));
    else
      return _Sentinel<false>(r::end(_M_base));
  }

  constexpr auto end() const
      requires detail::range_with_movable_reference<const _Vp> {
    if constexpr (r::common_range<const _Vp> && r::sized_range<const _Vp>)
      return _Iterator<true>(r::end(_M_base), r::distance(_M_base));
    else
      return _Sentinel<true>(r::end(_M_base));
  }

  constexpr auto size() requires r::sized_range<_Vp> {
    return r::size(_M_base);
  }

  constexpr auto size() const requires r::sized_range<const _Vp> {
    return r::size(_M_base);
  }

  constexpr _Vp base() const& requires copy_constructible<_Vp> {
    return _M_base;
  }

  constexpr _Vp base() && { return std::move(_M_base); }
};

template <typename _Range>
enumerate_view(_Range&&) -> enumerate_view<std::views::all_t<_Range>>;

template <r::view _Vp>
requires detail::range_with_movable_reference<_Vp> template <bool _Const>
class enumerate_view<_Vp>::_Iterator {
  using _Base = maybe_const_t<_Const, _Vp>;

  static auto _S_iter_concept() {
    if constexpr (r::random_access_range<_Base>)
      return random_access_iterator_tag{};
    else if constexpr (r::bidirectional_range<_Base>)
      return bidirectional_iterator_tag{};
    else if constexpr (r::forward_range<_Base>)
      return forward_iterator_tag{};
    else
      return input_iterator_tag{};
  }

  friend enumerate_view;

 public:
  using iterator_category = input_iterator_tag;
  using iterator_concept = decltype(_S_iter_concept());
  using difference_type = r::range_difference_t<_Base>;
  using value_type = tuple<difference_type, r::range_value_t<_Base>>;

 private:
  using reference_type = tuple<difference_type, r::range_reference_t<_Base>>;

  r::iterator_t<_Base> _M_current = r::iterator_t<_Base>();
  difference_type _M_pos = 0;

  constexpr explicit _Iterator(r::iterator_t<_Base> current,
                               difference_type pos)
      : _M_current(std::move(current)), _M_pos(pos) {}

 public:
  _Iterator() requires default_initializable<r::iterator_t<_Base>> = default;

  constexpr _Iterator(_Iterator<!_Const> i)
      requires _Const
                   && convertible_to<r::iterator_t<_Vp>, r::iterator_t<_Base>>
      : _M_current(std::move(i._M_current)), _M_pos(i._M_pos) {}

  constexpr const r::iterator_t<_Base>& base() const& noexcept {
    return _M_current;
  }

  constexpr r::iterator_t<_Base> base() && { return std::move(_M_current); }

  constexpr difference_type index() const noexcept { return _M_pos; }

  constexpr auto operator*() const {
    return reference_type(_M_pos, *_M_current);
  }

  constexpr _Iterator& operator++() {
    ++_M_current;
    ++_M_pos;
    return *this;
  }

  constexpr void operator++(int) { ++*this; }

  constexpr _Iterator operator++(int) requires r::forward_range<_Base> {
    auto tmp = *this;
    ++*this;
    return tmp;
  }

  constexpr _Iterator& operator--() requires r::bidirectional_range<_Base> {
    --_M_current;
    --_M_pos;
    return *this;
  }

  constexpr _Iterator operator--(int) requires r::bidirectional_range<_Base> {
    auto tmp = *this;
    --*this;
    return tmp;
  }

  constexpr _Iterator& operator+=(difference_type n)
      requires r::random_access_range<_Base> {
    _M_current += n;
    _M_pos += n;
    return *this;
  }

  constexpr _Iterator& operator-=(difference_type n)
      requires r::random_access_range<_Base> {
    _M_current -= n;
    _M_pos -= n;
    return *this;
  }

  constexpr auto operator[](difference_type n) const
      requires r::random_access_range<_Base> {
    return reference_type(_M_pos + n, _M_current[n]);
  }

  friend constexpr bool operator==(const _Iterator& x,
                                   const _Iterator& y) noexcept {
    return x._M_pos == y._M_pos;
  }

  friend constexpr strong_ordering operator<=>(const _Iterator& x,
                                               const _Iterator& y) noexcept {
    return x._M_pos <=> y._M_pos;
  }

  friend constexpr _Iterator operator+(const _Iterator& x, difference_type y)
      requires r::random_access_range<_Base> {
    return (_Iterator(x) += y);
  }

  friend constexpr _Iterator operator+(difference_type x, const _Iterator& y)
      requires r::random_access_range<_Base> {
    return _Iterator(y) += x;
  }

  friend constexpr _Iterator operator-(const _Iterator& x, difference_type y)
      requires r::random_access_range<_Base> {
    return _Iterator(x) -= y;
  }

  friend constexpr difference_type operator-(const _Iterator& x,
                                             const _Iterator& y) {
    return x._M_pos - y._M_pos;
  }

  friend constexpr auto iter_move(const _Iterator& i) noexcept(
      noexcept(r::iter_move(i._M_current)) &&
      is_nothrow_move_constructible_v<r::range_rvalue_reference_t<_Base>>) {
    return tuple<difference_type, r::range_rvalue_reference_t<_Base>>(
        i._M_pos, r::iter_move(i._M_current));
  }
};

template <r::view _Vp>
requires detail::range_with_movable_reference<_Vp> template <bool _Const>
class enumerate_view<_Vp>::_Sentinel {
  using _Base = maybe_const_t<_Const, _Vp>;

  r::sentinel_t<_Base> _M_end = r::sentinel_t<_Base>();

  constexpr explicit _Sentinel(r::sentinel_t<_Base> end)
      : _M_end(std::move(end)) {}

  friend enumerate_view;

 public:
  _Sentinel() = default;

  constexpr _Sentinel(_Sentinel<!_Const> other)
      requires _Const
               && convertible_to<r::sentinel_t<_Vp>, r::sentinel_t<_Base>>
      : _M_end(std::move(other._M_end)) {}

  constexpr r::sentinel_t<_Base> base() const { return _M_end; }

  template <bool _OtherConst>
  requires sentinel_for<r::sentinel_t<_Base>,
                        r::iterator_t<maybe_const_t<_OtherConst, _Vp>>>
  friend constexpr bool operator==(const _Iterator<_OtherConst>& x,
                                   const _Sentinel& y) {
    return x._M_current == y._M_end;
  }

  template <bool _OtherConst>
  requires sized_sentinel_for<r::sentinel_t<_Base>,
                              r::iterator_t<maybe_const_t<_OtherConst, _Vp>>>
  friend constexpr r::range_difference_t<maybe_const_t<_OtherConst, _Vp>>
  operator-(const _Iterator<_OtherConst>& x, const _Sentinel& y) {
    return x._M_current - y._M_end;
  }

  template <bool _OtherConst>
  requires sized_sentinel_for<r::sentinel_t<_Base>,
                              r::iterator_t<maybe_const_t<_OtherConst, _Vp>>>
  friend constexpr r::range_difference_t<maybe_const_t<_OtherConst, _Vp>>
  operator-(const _Sentinel& x, const _Iterator<_OtherConst>& y) {
    return x._M_end - y._M_current;
  }
};

namespace views {
namespace detail {
template <typename _Tp>
concept can_enumerate_view =
    requires { enumerate_view<r::views::all_t<_Tp>>(std::declval<_Tp>()); };
}

// TODO<joka921> Check if we need the rangeAdaptorClosures...
struct _Enumerate : __adaptor::_RangeAdaptorClosure {
  template <r::viewable_range _Range>
  requires detail::can_enumerate_view<_Range>
  constexpr auto operator() [[nodiscard]] (_Range&& r) const {
    return enumerate_view<r::views::all_t<_Range>>(std::forward<_Range>(r));
  }
};

inline constexpr _Enumerate enumerate;
}  // namespace views
}  // namespace ad_utility::ranges

template <typename _Tp>
inline constexpr bool std::ranges::enable_borrowed_range<
    ad_utility::ranges::enumerate_view<_Tp>> =
    std::ranges::enable_borrowed_range<_Tp>;
