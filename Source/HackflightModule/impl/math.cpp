#include <stdint.h>

extern "C" {

float atan2_approx(float y, float x)
{
    return atan2(y, x);
}

float acos_approx(float x)
{
    return acos(x);
}

} // extern "C"
