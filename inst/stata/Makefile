UNFD = ../../src
UNFPD = ../standalone
CC	= gcc
CXX 	= g++
CCW	= i586-mingw32-gcc
CXXW	= i586-mingw32-g++
# note if you are compiling on 64bit system for 32bit Stata, need -m32 as part of CFLAGS and CXXFLAGS 
# use -fPIC for 64 bit
CFLAGS	= -O2 -Wall -I$(UNFD) -DSYSTEM=OPUNIX  -fPIC
CWFLAGS	= -O2 -Wall -I$(UNFD) -DICONV18 -DBUILDING_LIBICONV  
CWXXFLAGS	= -O2 -DICONV18 -DBUILDING_LIBICONV -Wall -I$(UNFD) -I$(UNFPD) -DINACCURATE_SPRINTF -DFORCELOCALE 
CXXFLAGS	= -O2 -Wall -I$(UNFD) -I$(UNFPD) -DINACCURATE_SPRINTF -DFORCELOCALE  -DSYSTEM=OPUNIX -fPIC
#path to mingw
MINGWPATH=~/CrossR/cross-tools/bin/

default:: unf.plugin 

clean::
	rm -f unf.plugin unfw.plugin *.o

$(UNFD)/md5.o: $(UNFD)/md5.c $(UNFD)/md5.h
$(UNFD)/unf.o: $(UNFD)/unf.C $(UNFD)/unf.h
$(UNFD)/sha256.o: $(UNFD)/sha256.c $(UNFD)/sha256.h
$(UNFPD)/unf_plugin.o: $(UNFPD)/unf_plugin.C $(UNFPD)/unf_plugin.h

unf.plugin: ST_unf.o stplugin.o $(UNFD)/md5.o $(UNFD)/unf.o $(UNFD)/sha256.o $(UNFPD)/unf_plugin.o 
	$(CXX) $(CXXFLAGS) -shared $(UNFD)/md5.o $(UNFD)/sha256.o $(UNFD)/unf.o $(UNFPD)/unf_plugin.o stplugin.o ST_unf.o -o unf.plugin

unfw.plugin::
	export PATH=$(MINGWPATH):$(PATH);\
	$(CXXW) -c $(CWXXFLAGS) $(UNFD)/unf.C $(UNFPD)/unf_plugin.C ST_unf.C ;\
	$(CCW) -c $(CWFLAGS) $(UNFD)/md5.c $(UNFD)/sha256.c  stplugin.c ;\
        $(CXXW)  $(CWXXFLAGS) -shared -o unfw.plugin  unf.o stplugin.o sha256.o ST_unf.o unf_plugin.o md5.o -liconv 
