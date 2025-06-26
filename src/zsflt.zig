pub fn Float(comptime exponent_bits: u16, comptime mantissa_bits: u16) type {
    return packed struct {
        const Flt = @This();
        pub const Mantissa = std.meta.Int(.unsigned, mantissa_bits);
        pub const Exponent = std.meta.Int(.signed, exponent_bits);
        pub const BiasedExponent = enum(std.meta.Int(.unsigned, exponent_bits)) {
            denormal = 0,
            zero = (1 << (exponent_bits - 1)) - 1,
            infinite = (1 << exponent_bits) - 1,
            _, 

            pub const Int = @typeInfo(BiasedExponent).@"enum".tag_type;

            pub inline fn bias(exponent: Exponent) BiasedExponent {
                return @enumFromInt(@as(Int, @bitCast(exponent)) +% @as(Int, @intFromEnum(BiasedExponent.zero)));
            }

            pub inline fn unbias(biased: BiasedExponent) Exponent {
                return switch (biased) {
                    .denormal => unreachable,
                    .infinite => unreachable,
                    else => @bitCast(@intFromEnum(biased) -% @intFromEnum(BiasedExponent.zero)),
                };
            }
        };

        mantissa: Mantissa,
        exponent: BiasedExponent,
        sign: u1,

        pub inline fn of(value: anytype) Flt {
            const ValueType = @TypeOf(value);
            const value_ty_info = @typeInfo(ValueType);

            return sw: switch (value_ty_info) {
                .comptime_float, .comptime_int => .ofFloat(std.math.floatExponentBits(f128), std.math.floatMantissaBits(f128), @bitCast(@as(f128, value))),
                .float => |ty| {
                    const FloatType = std.meta.Float(ty.bits);
                    const flt_value: FloatType = value;

                    break :sw .ofFloat(std.math.floatExponentBits(FloatType), std.math.floatMantissaBits(FloatType), @bitCast(flt_value));
                },
                .@"struct" => |st| {
                    if(st.layout != .@"packed" or !@hasDecl(ValueType, "Exponent") or !@hasDecl(ValueType, "Mantissa")) {
                        @compileError("invalid struct, it must be another floating point value");
                    }

                    const o_mantissa_bits = @bitSizeOf(@field(ValueType, "Mantissa"));
                    const o_exponent_bits = @bitSizeOf(@field(ValueType, "Exponent"));

                    if(ValueType != Float(o_exponent_bits, o_mantissa_bits)) {
                        @compileError("invalid struct, it must be another floating point value");
                    }

                    return .ofFloat(o_exponent_bits, o_mantissa_bits, value);
                },
                else => @compileError(std.fmt.comptimePrint("cannot compute nearest floating point representation for {s}", .{@tagName(std.meta.activeTag(value_ty_info))})),
            };
        }

        fn ofFloat(comptime o_exponent_bits: u16, comptime o_mantissa_bits: u16, other: Float(o_exponent_bits, o_mantissa_bits)) Flt {
            if(exponent_bits == o_exponent_bits and mantissa_bits == o_mantissa_bits) {
                return other;
            }
            
            return switch (other.exponent) {
                .denormal => if(other.mantissa == 0)
                    .{ .mantissa = 0, .exponent = .denormal, .sign = other.sign }
                else 
                    @panic("TODO: subnormal floating values"),
                .infinite => .{
                    .mantissa = if(other.mantissa != 0) 1 else 0,
                    .exponent = .infinite,
                    .sign = other.sign,
                },
                else => flt: {
                    const unbiased = other.exponent.unbias();
                    const unbiased_converted = std.math.lossyCast(Flt.Exponent, unbiased);

                    break :flt switch (std.math.order(unbiased, unbiased_converted)) {
                        .lt => .{ .mantissa = 0, .exponent = .infinite, .sign = 1 - other.sign },
                        .gt => .{ .mantissa = 0, .exponent = .infinite, .sign = other.sign },
                        .eq => flt_eq: {
                            const biased_converted: Flt.BiasedExponent = .bias(unbiased_converted);
                            const mantissa_converted: Flt.Mantissa = switch (comptime std.math.order(o_mantissa_bits, @bitSizeOf(Flt.Mantissa))) {
                                .eq => @bitCast(other.mantissa),
                                .lt => @as(Flt.Mantissa, other.mantissa) << (@bitSizeOf(Flt.Mantissa) - o_mantissa_bits),
                                .gt => roundNearestEven(@TypeOf(other.mantissa), other.mantissa, o_mantissa_bits - @bitSizeOf(Flt.Mantissa)),
                            };

                            break :flt_eq .{ .mantissa = mantissa_converted, .exponent = biased_converted, .sign = other.sign };
                        }
                    };
                },
            };
        }

        fn roundNearestEven(comptime T: type, mantissa: T, comptime discarded_mantisa_bits: u16) Flt.Mantissa {
            const DiscardedInt = std.meta.Int(.unsigned, discarded_mantisa_bits);
            const lsb_bit = @as(T, 1) << @intCast(discarded_mantisa_bits);
            const round_bit = @as(DiscardedInt, 1) << @intCast(discarded_mantisa_bits - 1);
            const sticky_mask = std.math.maxInt(std.meta.Int(.unsigned, discarded_mantisa_bits - 1));

            const truncated: Flt.Mantissa = @truncate(mantissa >> @intCast(discarded_mantisa_bits));
            const round = @intFromBool(!((mantissa & round_bit == 0) or ((mantissa & sticky_mask) == 0 and (mantissa & lsb_bit) == 0)));

            return truncated +% round;
        }
    };
}

const testing = std.testing;

fn testBuiltinConversion(comptime A: type, comptime B: type, comptime AT: type, comptime BT: type, value: AT) !void {
    const zsflt_bitcasted: A = .of(value);
    const zsflt_converted: B = .of(zsflt_bitcasted);

    const hw_converted: BT = @floatCast(value);
    const zsflt_hw_converted: B = @bitCast(hw_converted);
    
    const zsflt_converted_as_hw: BT = @bitCast(zsflt_converted);

    try testing.expectEqualDeep(zsflt_hw_converted, zsflt_converted);
    try testing.expectApproxEqAbs(hw_converted, zsflt_converted_as_hw, std.math.floatEps(BT) * 0.1);
}

test Float {
    const F64 = Float(std.math.floatExponentBits(f64), std.math.floatMantissaBits(f64));
    const F32 = Float(std.math.floatExponentBits(f32), std.math.floatMantissaBits(f32));
    const F16 = Float(std.math.floatExponentBits(f16), std.math.floatMantissaBits(f16));
    
    try testBuiltinConversion(F32, F16, f32, f16, 1.2345678);
    try testBuiltinConversion(F64, F32, f64, f32, 8000000.8234567);
    try testBuiltinConversion(F64, F16, f64, f16, 80.8234);
    try testBuiltinConversion(F16, F32, f16, f32, 8.12234);
}

const std = @import("std");
