//
// Created by johannes on 26.11.19.
//


#include <hpx/hpx_main.hpp>
#include <hpx/include/iostreams.hpp>
#include <hpx/parallel/algorithm.hpp>
#include <cmath>
#include <vector>
#include <random>
#include <algorithm>


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



int main(int argc, char **argv)
{

  hpx::cout << "starting hpxMain main" << std::endl;

  auto x = randomSizeT(1 << 28ul, 5000);
  decltype(x) y(x.size());
  auto filter_pred = [](const auto& x) {return x < 2500;};
  hpx::cout << "starting remove" << std::endl;
  auto res = hpx::parallel::remove_copy_if(hpx::parallel::execution::par(hpx::parallel::execution::task), x.begin(), x.end(), y.begin(), filter_pred);
  y.resize(res.get().second - y.begin());
  hpx::cout << "first El" << y[0] << std::endl;
  hpx::cout << "size " << y.size() << std::endl;
  hpx::cout << "size " <<x.size() << std::endl;
  hpx::cout << "after sort hpxMain main" << std::endl;

  return 0;

}

