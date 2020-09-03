#include "ModuleManager.h"
IMPLEMENT_MODULE(FDefaultModuleImpl, FlightModule)

#include <hackflight.hpp>
#include "../MainModule/OSD.hpp"

void hf::Board::outbuf(char* msg)
{
	osd(msg);
}
