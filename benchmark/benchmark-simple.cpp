//
// Created by johannes on 25.11.19.
//


#include <hpx/hpx_init.hpp>
#include <hpx/parallel/algorithm.hpp>
#include <benchmark/benchmark.h>
#include <cmath>
#include <vector>
#include <random>
#include <algorithm>
#include <deque>
//#include <parallel/algorithm>

double SomeFunction() {
  return std::sqrt(2.0);
}

template <size_t A>
std::vector<std::array<size_t, A>> randomVector(size_t size, size_t upper) {
  // First create an instance of an engine.
  std::random_device rnd_device;
  // Specify the engine and distribution.
  std::mt19937 mersenne_engine {rnd_device()};  // Generates random integers
  std::uniform_int_distribution<size_t> dist {0, upper};

  auto gen = [&dist, &mersenne_engine](){
    std::array<size_t, A> res;
    for (auto& x : res) {
      x = dist(mersenne_engine);
    }
    return res;
  };

  std::vector<std::array<size_t, A>> vec(size);

  std::generate(std::begin(vec), std::end(vec), gen);
  return vec;
}

auto randomSizeT(size_t size, size_t upper) {
  // First create an instance of an engine.
  std::random_device rnd_device;
  // Specify the engine and distribution.
  std::mt19937 mersenne_engine {rnd_device()};  // Generates random integers
  std::uniform_int_distribution<size_t> dist {0, upper};

  auto gen = [&dist, &mersenne_engine](){
    return dist(mersenne_engine);
  };

  std::vector<size_t> vec(size);

  std::generate(std::begin(vec), std::end(vec), gen);
  return vec;
}

template <size_t A>
using Vec = std::vector<std::array<size_t, A>>;

template <size_t A>
auto filter(const Vec<A>& inp, const size_t threshold) {
  auto pred = [threshold](const auto& a) {return a[0] >= threshold;};
  Vec<A> res;
  res.reserve(inp.size());
  //hpx::parallel::remove_copy_if(hpx::parallel::execution::par, inp.begin(), inp.end(), std::back_inserter(res), pred);
  std::remove_copy_if(inp.begin(), inp.end(), std::back_inserter(res), pred);
  return res;

}

template <size_t A>
auto filter_hpx(const Vec<A>& inp, const size_t threshold) {
  auto pred = [threshold](const auto& a) {return a[0] >= threshold;};
  Vec<A> res;
  res.reserve(inp.size());
  auto it = hpx::parallel::remove_copy_if(hpx::parallel::execution::par, inp.begin(), inp.end(), res.begin(), pred);
  res.resize(it.second - res.begin());

  return res;

}



template <size_t A>
auto filter_reserve(Vec<A>& inp, size_t threshold) {
  Vec<A> res;
  res.reserve(inp.size());
  for (const auto& s : inp) {
    if (s[0] < threshold) {
      res.push_back(s);
    }
  }
  return res;

}


template <size_t A>
auto filter_branchless(const Vec<A>& inp, size_t threshold) {
  Vec<A> res(inp.size());
  size_t idx = 0;
  for (const auto& s : inp) {
    res[idx] = s;
    idx += s[0] < threshold;
  }
  res.resize(idx);
  return res;

}


static constexpr size_t sz = 5;

static void BM_SomeFunction(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();
    auto y = filter(x, 25000);
  }
}

static void BM_Filter_hpx(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();
    auto y = filter_hpx(x, 25000);
  }
}

static void BM_COPY (benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0)/2, 50000);
    auto y = randomVector<sz>(state.range(0)/2, 50000);
    state.ResumeTiming();
    decltype(x) z;
    z.reserve(x.size() + y.size());
    z.insert(z.end(), x.begin(), x.end());
    z.insert(z.end(), y.begin(), y.end());
  }
}

static void BM_MEMCOPY (benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0)/2, 50000);
    auto y = randomVector<sz>(state.range(0)/2, 50000);
    decltype(x) z;
    z.reserve(x.size() + y.size());
    state.ResumeTiming();
    z.insert(z.end(), x.begin(), x.end());
    z.insert(z.end(), y.begin(), y.end());
  }
}

static void BM_Branchless(benchmark::State& state) {
  // Perform setup here
  for (auto _ : state) {
    // This code gets timed
    state.PauseTiming();
    auto inp = randomVector<sz>(state.range(0), 50000);

    Vec<sz> res(inp.size());

    state.ResumeTiming();
    size_t idx = 0;
    size_t threshold = 1000;
    for (const auto &s : inp) {
      res[idx] = s;
      idx += s[0] < threshold;
    }
    res.resize(idx);
  }
}

static void BM_Reserve(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();
    auto y = filter_reserve(x, 1000);
  }
}

static void BM_Sort(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();
    std::sort(x.begin(), x.end(), [](const auto& a, const auto&b){return a[0] < b[0];});
  }
}

static void BM_HPX_Sort(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomSizeT(state.range(0), 50000);
    state.ResumeTiming();
    hpx::parallel::sort(hpx::parallel::execution::par(hpx::parallel::execution::task), x.begin(), x.end()).get();
  }
}

static void BM_HPX_SortPar(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomSizeT(state.range(0), 50000);
    state.ResumeTiming();
    hpx::parallel::sort(hpx::parallel::execution::par, x.begin(), x.end());
  }
}

static void BM_parallel(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();

    //__gnu_parallel::sort(x.begin(), x.end(), [](const auto& a, const auto&b){return a[0] < b[0];}, __gnu_parallel::parallel_tag(8));

  }
}

static void BM_partial(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();

    //__gnu_parallel::partial_sort(x.begin(), x.end(), x.end(), [](const auto& a, const auto&b){return a[0] < b[0];});
  }

}

static void BM_Top_100(benchmark::State& state) {
  // Perform setup here
  for (auto s : state) {
    // This code gets timed
    state.PauseTiming();
    auto x = randomVector<sz>(state.range(0), 50000);
    state.ResumeTiming();

   // __gnu_parallel::partial_sort(x.begin(), x.begin() + 100, x.end(), [](const auto& a, const auto&b){return a[0] < b[0];});
  }


}

// Register the function as a benchmark
BENCHMARK(BM_COPY)->Range(1<<15, 1ul << 27);
BENCHMARK(BM_Filter_hpx)->Range(1<<15, 1ul << 27ul);
BENCHMARK(BM_SomeFunction)->Range(1<<26, 1ul << 28ul);
BENCHMARK(BM_HPX_Sort)->Range(1ul << 23ul, 1ul << 28ul);
BENCHMARK(BM_HPX_SortPar)->Range(1ul << 23ul, 1ul << 28ul);
BENCHMARK(BM_Branchless)->Range(1024, 1ul << 26ul);
BENCHMARK(BM_parallel)->Range(1024, 1ul << 26ul);
BENCHMARK(BM_Top_100)->Range(1024, 1ul << 26ul);
BENCHMARK(BM_Sort)->Range(1024, 1ul << 26ul);
BENCHMARK(BM_partial)->Range(1024, 1ul << 26ul);
BENCHMARK(BM_Reserve)->Range(1024, 1ul << 26ul);
// Run the benchmark


int hpx_main(int argc, char **argv)
{
  benchmark::RunSpecifiedBenchmarks();

  return hpx::finalize();
}

int main(int argc, char **argv)
{
  benchmark::Initialize(&argc, argv);

  const char *env = std::getenv("NUM_THREADS");
  std::string thread_env = env == nullptr ? "1": std::string(env);
  thread_env = "8";

  std::vector<std::string> const cfg = {
          "hpx.os_threads!=" + thread_env
  };
  return hpx::init(argc, argv, cfg);
}
