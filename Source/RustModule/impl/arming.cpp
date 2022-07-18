#include <datatypes.h>

extern "C" {

    bool armingIsArmed(arming_t * armed)
    {
        (void)armed;
        return true;
    }

} // extern "C"
