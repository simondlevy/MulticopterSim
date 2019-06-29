#include "sockets/UdpServerSocket.hpp"
#include "sockets/UdpClientSocket.hpp"

int main(int argc, char ** argv)
{
    return 0;
}

/*
class FMatlabManager : public FFlightManager {

    private:

		const char * HOST = "127.0.0.1";
        const short MOTOR_PORT = 5000;
		const short TELEM_PORT = 5001;

        UdpServerSocket * _motorServer = NULL;
		UdpClientSocket * _telemClient = NULL;


        uint32_t _count = 0;

    public:

        FMatlabManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation) : 
            FFlightManager(dynamics, initialLocation, initialRotation)
        {
            _motorServer = new UdpServerSocket(MOTOR_PORT);
			_telemClient = new UdpClientSocket(HOST, TELEM_PORT);
            _count = 0;
        }
		
        ~FMatlabManager()
        {
            _motorServer->closeConnection();
            delete _motorServer;
            _motorServer = NULL;

			_telemClient->closeConnection();
			delete _telemClient;
			_telemClient = NULL;

        }

        virtual void update(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            // Avoid null-pointer exceptions at startup
            if (!_motorServer || !_telemClient) {
                return;
            }

			_motorServer->receiveData(motorvals, 8 * _motorCount);

			double tmp = 0;
			_telemClient->sendData(&tmp, sizeof(double));

			//dbgprintf("%d", _count++);

        }

        virtual void getGimbal(float & roll, float &pitch, float & fov) override
        { 
            // For now, we just keep camera pointing forward
            roll = 0; 
            pitch = 0; 
            fov = 90;
        }

}; // MatlabManager
*/
