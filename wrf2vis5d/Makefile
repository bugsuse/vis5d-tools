# compaq flags

LIBNETCDF = -L${NETCDF}/lib -lnetcdf -lnetcdff -lm  # NetCDF 4.1.3 later  
LIBVIS5D = ${HOME}/tools/vis5d-5.2/src/binio.o ${HOME}/tools/vis5d-5.2/src/v5d.o
INCLUDE = -I${NETCDF}/include -I./
FC = ${FC}
FCFLAGS = -g -C -free 

# linux flags

#LIBNETCDF = -L/usr/local/netcdf/lib -lnetcdf -lm
#LIBVIS5D = /usr/local/vis5d/src/binio.o /usr/local/vis5d/src/v5d.o
#INCLUDE = -I/usr/local/netcdf/include -I./
#FC = pgf90
#FCFLAGS = -g -C -Mfree
#FCFLAGS = -fast -Mfree

# Sun flags

#LIBNETCDF = -L/usr/local/netcdf/lib -lnetcdf -lm
#LIBVIS5D = /usr/local/vis5d/src/binio.o /usr/local/vis5d/src/v5d.o
#INCLUDE = -I/usr/local/netcdf/include -I./
#FC = f90
#FCFLAGS = -g -C -free 

# SGI flags

#LIBNETCDF = -L/usr/local/netcdf/lib -lnetcdf -lm
#LIBVIS5D = /usr/local/vis5d/src/binio.o /usr/local/vis5d/src/v5d.o
#INCLUDE = -I/usr/local/netcdf/include -I./
#FC = f90
#FCFLAGS = -g -C -freeform

wrf_to_v5d: wrf_to_v5d.o module_wrf_to_v5d_netcdf.o module_wrf_to_v5d_util.o
	$(FC) $(FCFLAGS) -o wrf_to_v5d wrf_to_v5d.o \
                         module_wrf_to_v5d_netcdf.o \
			 module_map_utils.o \
                         module_wrf_to_v5d_util.o \
                         $(LIBNETCDF) $(LIBVIS5D)

wrf_to_v5d.o: wrf_to_v5d.F module_wrf_to_v5d_util.o
	$(FC) $(FCFLAGS) -c wrf_to_v5d.F $(INCLUDE)

module_wrf_to_v5d_util.o: module_wrf_to_v5d_util.F module_wrf_to_v5d_netcdf.o \
				module_map_utils.o
	$(FC) $(FCFLAGS) -c module_wrf_to_v5d_util.F $(INCLUDE)

module_map_utils.o:	module_map_utils.F
	$(FC) $(FCFLAGS) -c module_map_utils.F $(INCLUDE)

module_wrf_to_v5d_netcdf.o: module_wrf_to_v5d_netcdf.F
	$(FC) $(FCFLAGS) -c module_wrf_to_v5d_netcdf.F $(INCLUDE)

clean:	
	rm -f *.o *.mod wrf_to_v5d
