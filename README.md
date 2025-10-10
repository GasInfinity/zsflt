# zsflt

An IEEE soft-float + fixed point library, used extensively in [zitrus](https://github.com/GasInfinity/zitrus/tree/main).

## Coverage:
- Conversion between arbitrary IEEE754 floating point values with nearest-even rounding handling almost all denormal cases (except denormal -> normal where DAZ currently).
- Arbitrary Fixed-point values and conversion between floating point and fixed.
