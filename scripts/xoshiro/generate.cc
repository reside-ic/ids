#include <cpp11.hpp>
#include <dust/random/random.hpp>

[[cpp11::linking_to("dust")]]
[[cpp11::register]]
cpp11::writable::raws xoshiro_run(cpp11::raws r_state, int n) {
  using rng_state = dust::random::xoshiro128plus_state;
  constexpr int n_bytes_per_draw = sizeof(rng_state::int_type);

  rng_state state;
  memcpy(&state.state, RAW(r_state), rng_state::size() * n_bytes_per_draw);

  cpp11::writable::raws ret(n_bytes_per_draw * n);
  auto dest = RAW(ret);
  for (int i = 0; i < n; ++i) {
    const auto value = next(state);
    memcpy(dest, &value, n_bytes_per_draw);
    dest += n_bytes_per_draw;
  }

  ret.attr("dim") = cpp11::writable::integers{n_bytes_per_draw, n};

  return ret;
}
