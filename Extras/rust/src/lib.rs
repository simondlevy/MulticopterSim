#[repr(C)]
pub struct VehicleState {
    x:      f32,
    dx:     f32,
    y:      f32,
    dy:     f32,
    z:      f32,
    dz:     f32,
    phi:    f32,
    dphi:   f32,
    theta:  f32,
    dtheta: f32,
    psi:    f32,
    dpsi:   f32
}

#[repr(C)]
pub struct Demands {
    throttle: f32,
    roll:     f32,
    pitch:    f32,
    yaw:      f32
}

#[repr(C)]
pub struct Motors {
    m1: f32,
    m2: f32,
    m3: f32,
    m4: f32
}

fn _alt_hold(_demands : Demands, _vehicle_state : VehicleState) -> Demands {

    Demands { throttle:0.0, roll:0.0, pitch:0.0, yaw:0.0 }
}

#[no_mangle]
pub extern "C" fn get_motors(
    c_demands : *mut Demands, c_vehicle_state: *mut VehicleState) -> Motors {

    let z = -(unsafe { (*c_vehicle_state).z });

    let m = if z < 1.0 { 0.6 } else { 0.0 };

    let _demands = Demands {
        throttle:(unsafe { (*c_demands).throttle }),
        roll:0.0,
        pitch:0.0,
        yaw:0.0 };

	Motors { m1: m, m2: m, m3: m, m4: m }
}
