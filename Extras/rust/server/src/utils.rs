pub mod utils {

    pub fn constrain(value: f32, lo: f32, hi: f32) -> f32 {
        if value  < lo {lo} else if value > hi {hi} else {value}
    }

    pub fn constrain_abs(value : f32, limit : f32) -> f32 {
        constrain(value, -limit, limit)
    }

    pub fn fabs(value: f32) -> f32 {
        if value < 0.0 {-value} else {value}
    }

} // mod utils
