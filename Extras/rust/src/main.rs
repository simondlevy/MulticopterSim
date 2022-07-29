use std::net::UdpSocket;

//use datatypes::datatypes::AltHoldPid;
//use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;

//use hackflight::hackflight::run_hackflight2;

//pub mod alt_hold;
pub mod datatypes;
//pub mod hackflight;


fn main() -> std::io::Result<()> {

    fn read_double(buf:[u8; IN_BUF_SIZE], beg:usize) -> f64 {
        let mut dst = [0u8; 8];
        dst.clone_from_slice(&buf[beg..beg+8]);
        f64::from_le_bytes(dst)
    }

    /*
       let mut alt_hold_pid = AltHoldPid {
       error_integral: 0.0,
       in_band: false,
       target: 0.0
       };*/

    const IN_BUF_SIZE:usize = 17*8;  // 17 doubles in

    let motor_client_socket = UdpSocket::bind("0.0.0.0:5000")?;
    let telemetry_server_socket = UdpSocket::bind("127.0.0.1:5001")?;

    println!("Hit the Play button ...");

    loop {

        let mut in_buf = [0; IN_BUF_SIZE]; 
        let (_amt, src) = telemetry_server_socket.recv_from(&mut in_buf)?;

        //let mut dst = [0u8; 8];
        //let beg:usize = 0;
        //dst.clone_from_slice(&in_buf[beg..beg+8]);
        //let time = f64::from_le_bytes(dst);

        let time = read_double(in_buf, 0);

        println!("{}", time);

        //let time = read_double(in_buf, 0, 8);

        let _vehicle_state = VehicleState {
            x:0.0,
            dx:0.0,
            y:0.0,
            dy:0.0,
            z:0.0,
            dz:0.0,
            phi:0.0,
            dphi:0.0,
            theta:0.0,
            dtheta:0.0,
            psi:0.0,
            dpsi:0.0
        };

        let out_buf = [0; 4*8]; // 4 doubles out
        motor_client_socket.send_to(&out_buf, &src)?;

        /*
           let demands = Demands {

           throttle:0.0,
           roll:0.0,
           pitch:0.0,
           yaw:0.0
           };

           let (new_alt_hold_pid, _motors) =
           run_hackflight2(demands, vehicle_state, alt_hold_pid.clone());

           alt_hold_pid.error_integral = new_alt_hold_pid.error_integral;
         */
    }

}
