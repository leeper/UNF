UNFD = ../../src
CC	= gcc
CCW	= i586-mingw32-gcc
CXXW	= i586-mingw32-g++
CXX 	= g++
CWFLAGS	= -O2 -Wall -I$(UNFD) 
CFLAGS	= -O2 -Wall -I$(UNFD) 
CWXXFLAGS	= -O2 -DICONV18 -DBUILDING_LIBICONV -Wall -I$(UNFD) -DINACCURATE_SPRINTF -DFORCELOCALE 
CXXFLAGS	= -O2 -Wall -I$(UNFD) -DINACCURATE_SPRINTF -DFORCELOCALE  
#PATH to mingw
MINGWPATH=~/CrossR/cross-tools/bin/



default:: unfvector

clean::
	rm -f unfvector *.o unfvector.exe

$(UNFD)/md5.o: $(UNFD)/md5.c $(UNFD)/md5.h
$(UNFD)/sha256.o: $(UNFD)/sha256.c $(UNFD)/sha256.h
$(UNFD)/unf.o: $(UNFD)/unf.C $(UNFD)/unf.h

unfvector: unfvector.o $(UNFD)/md5.o $(UNFD)/unf.o  $(UNFD)/sha256.o  unf_plugin.o
	$(CXX) $(CFLAGS) $(UNFD)/md5.o  $(UNFD)/unf.o $(UNFD)/sha256.o unfvector.o unf_plugin.o -o unfvector

unfvectorstatic: unfvector.o $(UNFD)/UNF.so unf_plugin.o
	$(CXX) $(CFLAGS) -static -L/usr/local/lib64 unfvector.o unf_plugin.o -o unfvectorstatic   -L/usr/lib64/R/lib -lR $(UNFD)/UNF.so  

unfvector.exe::
	export PATH=$(MINGWPATH):$(PATH) ;\
	$(CXXW) -c $(CWXXFLAGS) $(UNFD)/unf.C unfvector.C unf_plugin.C ;\
	$(CCW)  -c $(CWFLAGS) $(UNFD)/sha256.c $(UNFD)/md5.c ;\
	$(CXXW)  $(CWXXFLAGS) -static  -o unfvector.exe unf.o unfvector.o md5.o sha256.o unf_plugin.o  -liconv

