pub mod datatypes {

    #[repr(C)]
    pub struct AltHoldPid {

        pub error_integral: f32,
        pub in_band: bool,
        pub target:  f32
    }

    #[repr(C)]
    pub struct Demands {
        pub throttle: f32,
        pub roll: f32,
        pub pitch: f32,
        pub yaw: f32
    } 

    #[repr(C)]
    pub struct VehicleState {

        pub x: f32,
        pub dx: f32,
        pub y: f32,
        pub dy: f32,
        pub z: f32,
        pub dz: f32,
        pub phi: f32,
        pub dphi: f32,
        pub theta: f32,
        pub dtheta: f32,
        pub psi: f32,
        pub dpsi: f32
    }

    #[repr(C)]
    pub struct Motors {

        pub m1: f32,
        pub m2: f32,
        pub m3: f32,
        pub m4: f32
    }
    
    #[repr(C)]
    pub struct Hackflight {

        pub demands: Demands,
        pub vehicle_state: VehicleState,
        pub alt_hold_pid: AltHoldPid,
        pub motors: Motors
    }

}
