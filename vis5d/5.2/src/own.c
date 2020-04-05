#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "api.h"
#include "vis5d.h"
#include "script.h"

main(void){
   vis5d_init_window("howdy", 20, 20, 400, 400);
   vis5d_init_begin( 0, 0);
   vis5d_set_current_display( 0 );
   vis5d_open_gridfile( 0, "small.v5d", 1);
   vis5d_init_display_values( 0, 0);
   vis5d_moveresize_3d_window( 0, 0, 0, 399, 399);
}

