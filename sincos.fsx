// compute sin/cos of any angle starting with small one using
// angle doubling/halving formulas and trigonometry of angles sum/difference

open System

let inline cos_from_sin (sn: float) : float =
    Math.Sqrt ((1.0 + sn)*(1.0 - sn))

let sc_doubled (sn: float, cs: float) =
    (sn * cs * 2.0), ((cs + sn) * (cs - sn))

let sc_halved (sn: float, cs: float) =
    Math.Sqrt ((1.0 - cs) * 0.5), Math.Sqrt ((1.0 + cs) * 0.5)

let sc_sum (sn_a: float, cs_a: float) (sn_b: float, cs_b: float) =
    (sn_a*cs_b + cs_a*sn_b), (cs_a*cs_b - sn_a*sn_b)

let sc_diff (sn_a: float, cs_a: float) (sn_b: float, cs_b: float) =
    (sn_a*cs_b - cs_a*sn_b), (cs_a*cs_b + sn_a*sn_b)

let sin_via_series (x: float) :float =
    x - x*x*x/(1.0*2.0*3.0) + x*x*x*x*x/(1.0*2.0*3.0*4.0*5.0) - x*x*x*x*x*x*x/(1.0*2.0*3.0*4.0*5.0*6.0*7.0)

// F.e., computing sin(30.71 degrees)

// sin(small angle) first via series expansion
let angle = 1.0 // degrees
let sin_1 = sin_via_series (angle*Math.PI/180.0)
let cos_1 = cos_from_sin sin_1

printfn "sin(1d) = %f, cos(1d) = %f" sin_1 cos_1

// exhausting integer part of the desired angle
let (sin_2, cos_2) = sc_doubled (sin_1, cos_1)
let (sin_4, cos_4) = sc_doubled (sin_2, cos_2)
let (sin_8, cos_8) = sc_doubled (sin_4, cos_4)
let (sin_16, cos_16) = sc_doubled (sin_8, cos_8)
let (sin_32, cos_32) = sc_doubled (sin_16, cos_16)
let (sin_30, cos_30) = sc_diff (sin_32, cos_32) (sin_2, cos_2)

// now binary search part for the rest of the angle
let (sin_p5, cos_p5) = sc_halved (sin_1, cos_1)
let (sin_30p5, cos_30p5) = sc_sum (sin_30, cos_30) (sin_p5, cos_p5) // less than 30.71

let (sin_p25, cos_p25) = sc_halved (sin_p5, cos_p5)
let (sin_30p75, cos_30p75) = sc_sum (sin_30p5, cos_30p5) (sin_p25, cos_p25) // more than 30.71

let (sin_p125, cos_p125) = sc_halved (sin_p25, cos_p25)
let (sin_30p625, cos_30p625) = sc_diff (sin_30p75, cos_30p75) (sin_p125, cos_p125) // less than 30.71

let (sin_p0625, cos_p0625) = sc_halved (sin_p125, cos_p125)
let (sin_30p6875, cos_30p6875) = sc_sum (sin_30p625, cos_30p625) (sin_p0625, cos_p0625) // less than 30.71

let (sin_p03125, cos_p03125) = sc_halved (sin_p0625, cos_p0625)
let (sin_30p71875, cos_30p71875) = sc_sum (sin_30p6875, cos_30p6875) (sin_p03125, cos_p03125) // more than 30.71
