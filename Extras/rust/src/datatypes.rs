pub mod datatypes {

    #[repr(C)]
    pub struct AltHoldPid{

        pub error_integral: f32,
        pub in_band:        bool,
        pub target:         f32,
        pub throttle:       f32
    }
}
