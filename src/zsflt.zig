pub fn Float(comptime exponent_bits: u16, comptime mantissa_bits: u16) type {
    return packed struct(std.meta.Int(.unsigned, 1 + exponent_bits + mantissa_bits)) {
        const Flt = @This();
        pub const Mantissa = std.meta.Int(.unsigned, mantissa_bits);
        pub const Exponent = std.meta.Int(.signed, exponent_bits);
        pub const BiasedExponent = enum(std.meta.Int(.unsigned, exponent_bits)) {
            denormal = 0,
            min_normal = 1,
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
                else => @compileError(std.fmt.comptimePrint("cannot compute nearest floating point representation for {t}", .{std.meta.activeTag(value_ty_info)})),
            };
        }

        fn ofFloat(comptime o_exponent_bits: u16, comptime o_mantissa_bits: u16, other: Float(o_exponent_bits, o_mantissa_bits)) Flt {
            if(exponent_bits == o_exponent_bits and mantissa_bits == o_mantissa_bits) {
                return other;
            }
            
            return flt: switch (other.exponent) {
                .denormal => switch(comptime std.math.order(o_exponent_bits, @bitSizeOf(Flt.Exponent))) {
                    .gt => .{ .mantissa = 0, .exponent = .denormal, .sign = other.sign },
                    .eq => .{ .mantissa = nearestMantissaRepresentation(@TypeOf(other.mantissa), other.mantissa), .exponent = .denormal, .sign = other.sign },
                    // TODO: denormal -> normal
                    .lt => .{ .mantissa = 0, .exponent = .denormal, .sign = other.sign }, 
                },
                .infinite => .{
                    .mantissa = if(other.mantissa != 0) 1 else 0,
                    .exponent = .infinite,
                    .sign = other.sign,
                },
                _, .min_normal, .zero => {
                    const unbiased = other.exponent.unbias();
                    
                    if(unbiased > std.math.maxInt(Flt.Exponent)) break :flt .{ .mantissa = 0, .exponent = .infinite, .sign = other.sign };

                    if(unbiased <= std.math.minInt(Flt.Exponent)) {
                        const OtherMantissa = @TypeOf(other.mantissa);
                        const mantissa_bias = (BiasedExponent.min_normal.unbias() - unbiased);
                        const biased_mantissa = std.math.shr(@TypeOf(other.mantissa), other.mantissa, mantissa_bias) | std.math.shl(OtherMantissa, 1, @bitSizeOf(OtherMantissa) - mantissa_bias);
                        break :flt .{ .mantissa = nearestMantissaRepresentation(@TypeOf(biased_mantissa), biased_mantissa), .exponent = .denormal, .sign = other.sign };
                    }

                    const unbiased_converted: Flt.Exponent = @intCast(unbiased);
                    const biased_converted: BiasedExponent = .bias(unbiased_converted);

                    break :flt .{ .mantissa = nearestMantissaRepresentation(@TypeOf(other.mantissa), other.mantissa), .exponent = biased_converted, .sign = other.sign };
                },
            };
        }

        fn nearestMantissaRepresentation(comptime T: type, other: T) Flt.Mantissa {
            return switch (comptime std.math.order(@bitSizeOf(T), @bitSizeOf(Flt.Mantissa))) {
                .eq => @bitCast(other),
                .lt => @as(Flt.Mantissa, other) << (@bitSizeOf(Flt.Mantissa) - @bitSizeOf(T)),
                .gt => roundNearestEven(T, other),
            };
        }

        fn roundNearestEven(comptime T: type, mantissa: T) Flt.Mantissa {
            std.debug.assert(@bitSizeOf(T) > @bitSizeOf(Flt.Mantissa));

            const discarded_mantissa_bits = @bitSizeOf(T) - @bitSizeOf(Flt.Mantissa);

            const DiscardedInt = std.meta.Int(.unsigned, discarded_mantissa_bits);
            const lsb_bit = @as(T, 1) << @intCast(discarded_mantissa_bits);
            const round_bit = @as(DiscardedInt, 1) << @intCast(discarded_mantissa_bits - 1);
            const sticky_mask = std.math.maxInt(std.meta.Int(.unsigned, discarded_mantissa_bits - 1));

            const truncated: Flt.Mantissa = @truncate(mantissa >> @intCast(discarded_mantissa_bits));
            const round = @intFromBool(!((mantissa & round_bit == 0) or ((mantissa & sticky_mask) == 0 and (mantissa & lsb_bit) == 0)));
            return truncated +% round;
        }
    };
}

pub fn Fixed(comptime signedness: std.builtin.Signedness, comptime integer_bits: u16, comptime fractional_bits: u16) type {
    const sign_bit = @intFromBool(signedness == .signed);
    const FixInt = std.meta.Int(signedness, sign_bit + integer_bits + fractional_bits);

    return packed struct(FixInt) {
        const Fix = @This();

        pub const Fractional = std.meta.Int(.unsigned, fractional_bits);
        pub const Integer = std.meta.Int(.unsigned, integer_bits);
        pub const Sign = std.meta.Int(.unsigned, sign_bit);

        fractional: Fractional,
        integer: Integer,
        sign: Sign,

        pub fn ofSaturating(value: anytype) Fix {
            const ValueType = @TypeOf(value);
            const value_ty_info = @typeInfo(ValueType);

            return sw: switch (value_ty_info) {
                .comptime_int => .ofSaturating(@as(f128, value)),
                .comptime_float => .ofSaturating(@as(f128, value)),
                .float => |ty| {
                    const FloatType = std.meta.Float(ty.bits);
                    const flt_value: FloatType = value;
                    
                    if(signedness == .unsigned) std.debug.assert(flt_value >= 0);

                    const scale: FloatType = @floatFromInt(1 << fractional_bits);
                    const scaled: FloatType = @round(flt_value * scale);

                    break :sw if(scaled >= std.math.maxInt(FixInt))
                        @bitCast(@as(FixInt, std.math.maxInt(FixInt)))
                    else if(signedness == .signed and scaled <= std.math.minInt(FixInt))
                        @bitCast(@as(FixInt, std.math.minInt(FixInt)))
                    else @bitCast(@as(FixInt, @intFromFloat(scaled)));
                },
                // TODO: Convert between fixed-points
                else => @compileError(std.fmt.comptimePrint("cannot compute nearest floating point representation for {t}", .{std.meta.activeTag(value_ty_info)})),
            };
        }

        pub fn add(a: Fix, b: Fix) Fix {
            return @bitCast(@as(FixInt, @bitCast(a)) + @as(FixInt, @bitCast(b)));
        }

        pub fn sub(a: Fix, b: Fix) Fix {
            return @bitCast(@as(FixInt, @bitCast(a)) - @as(FixInt, @bitCast(b)));
        }
    };
}

const testing = std.testing;

fn testComparingHardwareConversion(comptime A: type, comptime B: type, comptime AT: type, comptime BT: type, value: AT) !void {
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
    
    try testComparingHardwareConversion(F32, F16, f32, f16, 655445.348);
    try testComparingHardwareConversion(F32, F16, f32, f16, 65545.2446);
    try testComparingHardwareConversion(F32, F16, f32, f16, 50545.2467);
    try testComparingHardwareConversion(F32, F16, f32, f16, 20000.008);
    try testComparingHardwareConversion(F32, F16, f32, f16, 0.00008);
    try testComparingHardwareConversion(F32, F16, f32, f16, 0.2342);

    try testComparingHardwareConversion(F32, F16, f32, f16, 0.00002);
    try testComparingHardwareConversion(F32, F16, f32, f16, 0.0000002);
    try testComparingHardwareConversion(F32, F16, f32, f16, 0.0000000002);

    try testComparingHardwareConversion(F32, F16, f32, f16, std.math.inf(f32));
    try testComparingHardwareConversion(F32, F16, f32, f16, -std.math.inf(f32));

    try testComparingHardwareConversion(F64, F32, f64, f32, 8000000.8234567);
    try testComparingHardwareConversion(F64, F16, f64, f16, 80.8234);
    try testComparingHardwareConversion(F16, F32, f16, f32, 8.12234);
}

test Fixed {
    const UQ4_4 = Fixed(.unsigned, 4, 4);
    const Q3_4 = Fixed(.signed, 3, 4);

    const UQ0_11 = Fixed(.unsigned, 0, 11);

    try testing.expectEqual(@as(Q3_4, @bitCast(@as(u8, 0b0011_1000))), Q3_4.ofSaturating(3.5)); // -3.5 -> -3.5
    try testing.expectEqual(@as(Q3_4, @bitCast(@as(u8, 0b1000_0100))), Q3_4.ofSaturating(-7.742)); // -7.742 -> -7.750
    try testing.expectEqual(@as(Q3_4, @bitCast(@as(u8, 0b1110_0100))), Q3_4.ofSaturating(-1.742)); // -1.742 -> -1.750

    try testing.expectEqual(@as(UQ4_4, @bitCast(@as(u8, 0b1111_1010))), UQ4_4.ofSaturating(15.620)); // 15.620 -> 15.625

    try testing.expectEqual(@as(UQ0_11, @bitCast(@as(u11, 0b11111111111))), UQ0_11.ofSaturating(1.0)); // 1.0 -> 0.99951171875
}

const builtin = @import("builtin");
const std = @import("std");
