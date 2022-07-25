#[repr(C)]
pub struct VehicleState {
    x: f32,
    dx: f32,
    y: f32,
    dy: f32,
    z: f32,
    dz: f32,
    phi: f32,
    dphi: f32,
    theta: f32,
    dtheta: f32,
    psi: f32,
    dpsi: f32
}

#[repr(C)]
pub struct Motors {
    m1: f32,
    m2: f32,
    m3: f32,
    m4: f32
}

#[no_mangle]
pub extern "C" fn get_motors(vehicle_state: *mut VehicleState) -> Motors {

    let z = unsafe { (*vehicle_state).z };

    let _m = if z < 1.0 { 0.6 } else { 0.0 };

	Motors { m1: 0.6, m2: 0.6, m3: 0.6, m4:0.6 }
}
