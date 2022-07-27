pub mod datatypes {

    #[repr(C)]
    pub struct AltHoldPid {

        pub error_integral: f32,
        pub in_band: bool,
        pub target:  f32,
        pub throttle:f32
    }

    #[repr(C)]
    pub struct Demands {
        pub throttle: f32,
        pub roll: f32,
        pub pitch: f32,
        pub yaw: f32
    } 
}
