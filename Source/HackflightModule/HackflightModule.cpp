#include "ModuleManager.h"
IMPLEMENT_MODULE(FDefaultModuleImpl, HackflightModule)

#include <RoboFirmwareToolkit.hpp>
#include <RFT_board.hpp>
#include "../MainModule/OSD.hpp"

void rft::Board::outbuf(char* msg)
{
	// false = no error; true = overwrite
	osd(msg, false, true);
}
