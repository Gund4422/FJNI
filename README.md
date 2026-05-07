# FJNI 
The first ever Fortran binding for the Java Native Interface. 28 years of JNI existing, nobody did this. now someone did.

## Build Instructions

### On Windows - gfortran
- javac Bootstrap.java
- gcc -O3 -c jni_shim.c -I"%JAVA_HOME%\include" -I"%JAVA_HOME%\include\win32" -o jni_shim.o
- gfortran -O3 -c jni.f90 -o jni.o
- gfortran -O3 -c test_native.f90 -o test_native.o
- gfortran -O3 -shared -o fjni_core.dll jni_shim.o jni.o test_native.o -static-libgfortran -static-libgcc
- java -Djava.library.path=. --enable-native-access=ALL-UNNAMED Bootstrap

### On Windows - Intel ifx
- call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
- javac Bootstrap.java
- icl /O3 /c jni_shim.c /I"%JAVA_HOME%\include" /I"%JAVA_HOME%\include\win32" /Fojni_shim.obj
- ifx /O3 /c jni.f90 /object:jni.obj
- ifx /O3 /c test_native.f90 /object:test_native.obj
- link /DLL /OUT:fjni_core.dll jni_shim.obj jni.obj test_native.obj libifcoremd.lib libifport.lib
- java -Djava.library.path=. --enable-native-access=ALL-UNNAMED Bootstrap
- Add /libs:static to ifx commands if you don't want to ship Intel DLLs

### On Linux - gfortran
- javac -h . Bootstrap.java
- gcc -O3 -fPIC -c jni_shim.c -I"$JAVA_HOME/include" -I"$JAVA_HOME/include/linux" -o jni_shim.o
- gfortran -O3 -fPIC -c jni.f90 -o jni.o
- gfortran -O3 -fPIC -c test_native.f90 -o test_native.o
- gfortran -O3 -shared -o libfjni_core.so jni_shim.o jni.o test_native.o -static-libgfortran -static-libgcc
- java -Djava.library.path=. --enable-native-access=ALL-UNNAMED Bootstrap

### On Linux - Intel ifx
- source /opt/intel/oneapi/setvars.sh
- javac -h . Bootstrap.java
- icx -O3 -fPIC -c jni_shim.c -I"$JAVA_HOME/include" -I"$JAVA_HOME/include/linux" -o jni_shim.o
- ifx -O3 -fPIC -c jni.f90 -o jni.o
- ifx -O3 -fPIC -c test_native.f90 -o test_native.o
- ifx -O3 -shared -o libfjni_core.so jni_shim.o jni.o test_native.o -static-intel
- java -Djava.library.path=. --enable-native-access=ALL-UNNAMED Bootstrap

## Proof of it working.
![Fortran says 7](Capture.JPG)
