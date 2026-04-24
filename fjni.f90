! FJNI, kind of a full port of jni.h to fortran
! CREDITS: Oracle

module jni
  use iso_c_binding
  implicit none

  integer, parameter :: jboolean = c_int8_t       ! unsigned char  -> 8-bit int
  integer, parameter :: jbyte    = c_int8_t        ! signed byte
  integer, parameter :: jchar    = c_int16_t       ! unsigned short -> 16-bit
  integer, parameter :: jshort   = c_int16_t       ! short
  integer, parameter :: jint     = c_int32_t       ! int
  integer, parameter :: jlong    = c_int64_t       ! long
  integer, parameter :: jfloat   = c_float         ! float
  integer, parameter :: jdouble  = c_double        ! double
  integer, parameter :: jsize    = c_int32_t       ! jint alias

  integer(jboolean), parameter :: JNI_FALSE = 0
  integer(jboolean), parameter :: JNI_TRUE  = 1

  integer(c_int), parameter :: JNI_OK        =  0   ! success
  integer(c_int), parameter :: JNI_ERR       = -1   ! unknown error
  integer(c_int), parameter :: JNI_EDETACHED = -2   ! thread detached from VM
  integer(c_int), parameter :: JNI_EVERSION  = -3   ! JNI version error
  integer(c_int), parameter :: JNI_ENOMEM    = -4   ! not enough memory
  integer(c_int), parameter :: JNI_EEXIST    = -5   ! VM already created
  integer(c_int), parameter :: JNI_EINVAL    = -6   ! invalid arguments

  integer(c_int), parameter :: JNI_COMMIT = 1
  integer(c_int), parameter :: JNI_ABORT  = 2

  integer(c_int), parameter :: JNI_VERSION_1_1 = int(z'00010001')
  integer(c_int), parameter :: JNI_VERSION_1_2 = int(z'00010002')
  integer(c_int), parameter :: JNI_VERSION_1_4 = int(z'00010004')
  integer(c_int), parameter :: JNI_VERSION_1_6 = int(z'00010006')
  integer(c_int), parameter :: JNI_VERSION_1_8 = int(z'00010008')
  integer(c_int), parameter :: JNI_VERSION_9   = int(z'00090000')
  integer(c_int), parameter :: JNI_VERSION_10  = int(z'000a0000')
  integer(c_int), parameter :: JNI_VERSION_19  = int(z'00130000')
  integer(c_int), parameter :: JNI_VERSION_20  = int(z'00140000')
  integer(c_int), parameter :: JNI_VERSION_21  = int(z'00150000')
  integer(c_int), parameter :: JNI_VERSION_24  = int(z'00180000')

  integer(c_int), parameter :: JNIInvalidRefType    = 0
  integer(c_int), parameter :: JNILocalRefType      = 1
  integer(c_int), parameter :: JNIGlobalRefType     = 2
  integer(c_int), parameter :: JNIWeakGlobalRefType = 3

  ! ============================================================
  ! Opaque Object Reference Types
  ! All Java object references are opaque C pointers (void*)
  ! In Fortran, represent as type(c_ptr)
  ! ============================================================

  ! Base object types — all are c_ptr under the hood
  ! These are type aliases for documentation clarity.
  ! Use type(c_ptr) for all of: jobject, jclass, jthrowable,
  ! jstring, jarray and all array subtypes, jweak.

  type, bind(C) :: JNINativeMethod
    type(c_ptr) :: name       ! char*
    type(c_ptr) :: signature  ! char*
    type(c_ptr) :: fnPtr      ! void*
  end type JNINativeMethod

  type, bind(C) :: JavaVMOption
    type(c_ptr) :: optionString  ! char*
    type(c_ptr) :: extraInfo     ! void*
  end type JavaVMOption

  type, bind(C) :: JavaVMInitArgs
    integer(c_int32_t) :: version
    integer(c_int32_t) :: nOptions
    type(c_ptr)        :: options            ! JavaVMOption*
    integer(c_int8_t)  :: ignoreUnrecognized ! jboolean
  end type JavaVMInitArgs

  type, bind(C) :: JavaVMAttachArgs
    integer(c_int32_t) :: version
    type(c_ptr)        :: name   ! char*
    type(c_ptr)        :: group  ! jobject
  end type JavaVMAttachArgs

  ! ============================================================
  ! jvalue — discriminated union
  ! Fortran does not have unions; use a fixed-size sequence type
  ! large enough for the largest member (jdouble / jlong = 8 bytes)
  ! Access the desired field by transferring bits with TRANSFER().
  ! ============================================================

  type, bind(C) :: jvalue
    integer(c_int8_t) :: raw(8)  ! 8-byte raw storage
  end type jvalue

  integer, parameter :: jobjectRefType = c_int

contains

  pure function jvalue_z(v) result(jv)
    integer(c_int8_t), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = 0_c_int8_t
    jv%raw(1) = v
  end function jvalue_z

  pure function jvalue_b(v) result(jv)
    integer(c_int8_t), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = 0_c_int8_t
    jv%raw(1) = v
  end function jvalue_b

  pure function jvalue_s(v) result(jv)
    integer(c_int16_t), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = transfer(v, jv%raw)
  end function jvalue_s

  pure function jvalue_i(v) result(jv)
    integer(c_int32_t), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = 0_c_int8_t
    jv%raw(1:4) = transfer(v, jv%raw(1:4))
  end function jvalue_i

  pure function jvalue_j(v) result(jv)
    integer(c_int64_t), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = transfer(v, jv%raw)
  end function jvalue_j

  pure function jvalue_f(v) result(jv)
    real(c_float), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = 0_c_int8_t
    jv%raw(1:4) = transfer(v, jv%raw(1:4))
  end function jvalue_f

  pure function jvalue_d(v) result(jv)
    real(c_double), intent(in) :: v
    type(jvalue) :: jv
    jv%raw = transfer(v, jv%raw)
  end function jvalue_d

  pure function jvalue_l(v) result(jv)
    type(c_ptr), intent(in) :: v   ! jobject
    type(jvalue) :: jv
    jv%raw = transfer(v, jv%raw)
  end function jvalue_l

end module jni

module jni_invocation
  use iso_c_binding
  use jni, only: JavaVMInitArgs, JavaVMAttachArgs
  implicit none

  interface

    ! jint JNI_GetDefaultJavaVMInitArgs(void *args)
    function JNI_GetDefaultJavaVMInitArgs(args) &
        bind(C, name="JNI_GetDefaultJavaVMInitArgs") result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: args
      integer(c_int)     :: rc
    end function

    ! jint JNI_CreateJavaVM(JavaVM **pvm, void **penv, void *args)
    function JNI_CreateJavaVM(pvm, penv, args) &
        bind(C, name="JNI_CreateJavaVM") result(rc)
      import :: c_ptr, c_int
      type(c_ptr) :: pvm   ! JavaVM**  (out)
      type(c_ptr) :: penv  ! JNIEnv**  (out)
      type(c_ptr), value :: args
      integer(c_int) :: rc
    end function

    ! jint JNI_GetCreatedJavaVMs(JavaVM **vmBuf, jsize bufLen, jsize *nVMs)
    function JNI_GetCreatedJavaVMs(vmBuf, bufLen, nVMs) &
        bind(C, name="JNI_GetCreatedJavaVMs") result(rc)
      import :: c_ptr, c_int, c_int32_t
      type(c_ptr), value    :: vmBuf
      integer(c_int32_t), value :: bufLen
      type(c_ptr), value    :: nVMs
      integer(c_int)        :: rc
    end function

  end interface

end module jni_invocation

module jni_env
  use iso_c_binding
  use jni
  implicit none

  interface

    ! --- Version ---
    function jni_GetVersion(env) bind(C, name="jni_GetVersion") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env
      integer(c_int32_t) :: r
    end function

    ! --- Class Operations ---
    function jni_FindClass(env, name) bind(C, name="jni_FindClass") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env
      character(c_char)  :: name(*)
      type(c_ptr)        :: r   ! jclass
    end function

    function jni_GetSuperclass(env, sub) bind(C, name="jni_GetSuperclass") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, sub
      type(c_ptr)        :: r
    end function

    function jni_IsAssignableFrom(env, sub, sup) &
        bind(C, name="jni_IsAssignableFrom") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, sub, sup
      integer(c_int8_t)  :: r
    end function

    ! --- Exceptions ---
    function jni_Throw(env, obj) bind(C, name="jni_Throw") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, obj
      integer(c_int32_t) :: r
    end function

    function jni_ThrowNew(env, clazz, msg) bind(C, name="jni_ThrowNew") result(r)
      import :: c_ptr, c_char, c_int32_t
      type(c_ptr), value :: env, clazz
      character(c_char)  :: msg(*)
      integer(c_int32_t) :: r
    end function

    function jni_ExceptionOccurred(env) &
        bind(C, name="jni_ExceptionOccurred") result(r)
      import :: c_ptr
      type(c_ptr), value :: env
      type(c_ptr)        :: r   ! jthrowable
    end function

    subroutine jni_ExceptionDescribe(env) &
        bind(C, name="jni_ExceptionDescribe")
      import :: c_ptr
      type(c_ptr), value :: env
    end subroutine

    subroutine jni_ExceptionClear(env) bind(C, name="jni_ExceptionClear")
      import :: c_ptr
      type(c_ptr), value :: env
    end subroutine

    function jni_ExceptionCheck(env) bind(C, name="jni_ExceptionCheck") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env
      integer(c_int8_t)  :: r
    end function

    subroutine jni_FatalError(env, msg) bind(C, name="jni_FatalError")
      import :: c_ptr, c_char
      type(c_ptr), value :: env
      character(c_char)  :: msg(*)
    end subroutine

    ! --- Global / Local References ---
    function jni_NewGlobalRef(env, lobj) bind(C, name="jni_NewGlobalRef") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, lobj
      type(c_ptr)        :: r
    end function

    subroutine jni_DeleteGlobalRef(env, gref) bind(C, name="jni_DeleteGlobalRef")
      import :: c_ptr
      type(c_ptr), value :: env, gref
    end subroutine

    subroutine jni_DeleteLocalRef(env, obj) bind(C, name="jni_DeleteLocalRef")
      import :: c_ptr
      type(c_ptr), value :: env, obj
    end subroutine

    function jni_NewLocalRef(env, ref) bind(C, name="jni_NewLocalRef") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, ref
      type(c_ptr)        :: r
    end function

    function jni_IsSameObject(env, obj1, obj2) &
        bind(C, name="jni_IsSameObject") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, obj1, obj2
      integer(c_int8_t)  :: r
    end function

    function jni_EnsureLocalCapacity(env, capacity) &
        bind(C, name="jni_EnsureLocalCapacity") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value    :: env
      integer(c_int32_t), value :: capacity
      integer(c_int32_t)    :: r
    end function

    function jni_PushLocalFrame(env, capacity) &
        bind(C, name="jni_PushLocalFrame") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value    :: env
      integer(c_int32_t), value :: capacity
      integer(c_int32_t)    :: r
    end function

    function jni_PopLocalFrame(env, result_obj) &
        bind(C, name="jni_PopLocalFrame") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, result_obj
      type(c_ptr)        :: r
    end function

    ! --- Weak References ---
    function jni_NewWeakGlobalRef(env, obj) &
        bind(C, name="jni_NewWeakGlobalRef") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, obj
      type(c_ptr)        :: r   ! jweak
    end function

    subroutine jni_DeleteWeakGlobalRef(env, ref) &
        bind(C, name="jni_DeleteWeakGlobalRef")
      import :: c_ptr
      type(c_ptr), value :: env, ref
    end subroutine

    function jni_GetObjectRefType(env, obj) &
        bind(C, name="jni_GetObjectRefType") result(r)
      import :: c_ptr, c_int
      type(c_ptr), value :: env, obj
      integer(c_int)     :: r   ! jobjectRefType enum
    end function

    ! --- Object Operations ---
    function jni_AllocObject(env, clazz) bind(C, name="jni_AllocObject") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, clazz
      type(c_ptr)        :: r
    end function

    function jni_GetObjectClass(env, obj) &
        bind(C, name="jni_GetObjectClass") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, obj
      type(c_ptr)        :: r   ! jclass
    end function

    function jni_IsInstanceOf(env, obj, clazz) &
        bind(C, name="jni_IsInstanceOf") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, obj, clazz
      integer(c_int8_t)  :: r
    end function

    ! --- Method IDs ---
    function jni_GetMethodID(env, clazz, name, sig) &
        bind(C, name="jni_GetMethodID") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env, clazz
      character(c_char)  :: name(*), sig(*)
      type(c_ptr)        :: r   ! jmethodID
    end function

    function jni_GetStaticMethodID(env, clazz, name, sig) &
        bind(C, name="jni_GetStaticMethodID") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env, clazz
      character(c_char)  :: name(*), sig(*)
      type(c_ptr)        :: r   ! jmethodID
    end function

    ! --- Field IDs ---
    function jni_GetFieldID(env, clazz, name, sig) &
        bind(C, name="jni_GetFieldID") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env, clazz
      character(c_char)  :: name(*), sig(*)
      type(c_ptr)        :: r   ! jfieldID
    end function

    function jni_GetStaticFieldID(env, clazz, name, sig) &
        bind(C, name="jni_GetStaticFieldID") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env, clazz
      character(c_char)  :: name(*), sig(*)
      type(c_ptr)        :: r   ! jfieldID
    end function

    ! --- Call Instance Methods (A = jvalue array variants) ---
    function jni_CallObjectMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallObjectMethodA") result(r)
      import :: c_ptr, jvalue
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      type(c_ptr)        :: r
    end function

    function jni_CallBooleanMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallBooleanMethodA") result(r)
      import :: c_ptr, jvalue, c_int8_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int8_t)  :: r
    end function

    function jni_CallByteMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallByteMethodA") result(r)
      import :: c_ptr, jvalue, c_int8_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int8_t)  :: r
    end function

    function jni_CallCharMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallCharMethodA") result(r)
      import :: c_ptr, jvalue, c_int16_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int16_t) :: r
    end function

    function jni_CallShortMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallShortMethodA") result(r)
      import :: c_ptr, jvalue, c_int16_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int16_t) :: r
    end function

    function jni_CallIntMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallIntMethodA") result(r)
      import :: c_ptr, jvalue, c_int32_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int32_t) :: r
    end function

    function jni_CallLongMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallLongMethodA") result(r)
      import :: c_ptr, jvalue, c_int64_t
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      integer(c_int64_t) :: r
    end function

    function jni_CallFloatMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallFloatMethodA") result(r)
      import :: c_ptr, jvalue, c_float
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      real(c_float)      :: r
    end function

    function jni_CallDoubleMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallDoubleMethodA") result(r)
      import :: c_ptr, jvalue, c_double
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
      real(c_double)     :: r
    end function

    subroutine jni_CallVoidMethodA(env, obj, mid, args) &
        bind(C, name="jni_CallVoidMethodA")
      import :: c_ptr, jvalue
      type(c_ptr), value :: env, obj, mid
      type(jvalue)       :: args(*)
    end subroutine

    ! --- Call Static Methods (A variants) ---
    function jni_CallStaticObjectMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticObjectMethodA") result(r)
      import :: c_ptr, jvalue
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      type(c_ptr)        :: r
    end function

    function jni_CallStaticBooleanMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticBooleanMethodA") result(r)
      import :: c_ptr, jvalue, c_int8_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int8_t)  :: r
    end function

    function jni_CallStaticByteMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticByteMethodA") result(r)
      import :: c_ptr, jvalue, c_int8_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int8_t)  :: r
    end function

    function jni_CallStaticCharMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticCharMethodA") result(r)
      import :: c_ptr, jvalue, c_int16_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int16_t) :: r
    end function

    function jni_CallStaticShortMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticShortMethodA") result(r)
      import :: c_ptr, jvalue, c_int16_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int16_t) :: r
    end function

    function jni_CallStaticIntMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticIntMethodA") result(r)
      import :: c_ptr, jvalue, c_int32_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int32_t) :: r
    end function

    function jni_CallStaticLongMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticLongMethodA") result(r)
      import :: c_ptr, jvalue, c_int64_t
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      integer(c_int64_t) :: r
    end function

    function jni_CallStaticFloatMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticFloatMethodA") result(r)
      import :: c_ptr, jvalue, c_float
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      real(c_float)      :: r
    end function

    function jni_CallStaticDoubleMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticDoubleMethodA") result(r)
      import :: c_ptr, jvalue, c_double
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
      real(c_double)     :: r
    end function

    subroutine jni_CallStaticVoidMethodA(env, clazz, mid, args) &
        bind(C, name="jni_CallStaticVoidMethodA")
      import :: c_ptr, jvalue
      type(c_ptr), value :: env, clazz, mid
      type(jvalue)       :: args(*)
    end subroutine

    ! --- Instance Field Getters ---
    function jni_GetObjectField(env, obj, fid) &
        bind(C, name="jni_GetObjectField") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, obj, fid
      type(c_ptr)        :: r
    end function

    function jni_GetBooleanField(env, obj, fid) &
        bind(C, name="jni_GetBooleanField") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int8_t)  :: r
    end function

    function jni_GetByteField(env, obj, fid) &
        bind(C, name="jni_GetByteField") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int8_t)  :: r
    end function

    function jni_GetCharField(env, obj, fid) &
        bind(C, name="jni_GetCharField") result(r)
      import :: c_ptr, c_int16_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int16_t) :: r
    end function

    function jni_GetShortField(env, obj, fid) &
        bind(C, name="jni_GetShortField") result(r)
      import :: c_ptr, c_int16_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int16_t) :: r
    end function

    function jni_GetIntField(env, obj, fid) &
        bind(C, name="jni_GetIntField") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int32_t) :: r
    end function

    function jni_GetLongField(env, obj, fid) &
        bind(C, name="jni_GetLongField") result(r)
      import :: c_ptr, c_int64_t
      type(c_ptr), value :: env, obj, fid
      integer(c_int64_t) :: r
    end function

    function jni_GetFloatField(env, obj, fid) &
        bind(C, name="jni_GetFloatField") result(r)
      import :: c_ptr, c_float
      type(c_ptr), value :: env, obj, fid
      real(c_float)      :: r
    end function

    function jni_GetDoubleField(env, obj, fid) &
        bind(C, name="jni_GetDoubleField") result(r)
      import :: c_ptr, c_double
      type(c_ptr), value :: env, obj, fid
      real(c_double)     :: r
    end function

    ! --- Instance Field Setters ---
    subroutine jni_SetObjectField(env, obj, fid, val) &
        bind(C, name="jni_SetObjectField")
      import :: c_ptr
      type(c_ptr), value :: env, obj, fid, val
    end subroutine

    subroutine jni_SetBooleanField(env, obj, fid, val) &
        bind(C, name="jni_SetBooleanField")
      import :: c_ptr, c_int8_t
      type(c_ptr), value    :: env, obj, fid
      integer(c_int8_t), value :: val
    end subroutine

    subroutine jni_SetByteField(env, obj, fid, val) &
        bind(C, name="jni_SetByteField")
      import :: c_ptr, c_int8_t
      type(c_ptr), value    :: env, obj, fid
      integer(c_int8_t), value :: val
    end subroutine

    subroutine jni_SetCharField(env, obj, fid, val) &
        bind(C, name="jni_SetCharField")
      import :: c_ptr, c_int16_t
      type(c_ptr), value     :: env, obj, fid
      integer(c_int16_t), value :: val
    end subroutine

    subroutine jni_SetShortField(env, obj, fid, val) &
        bind(C, name="jni_SetShortField")
      import :: c_ptr, c_int16_t
      type(c_ptr), value     :: env, obj, fid
      integer(c_int16_t), value :: val
    end subroutine

    subroutine jni_SetIntField(env, obj, fid, val) &
        bind(C, name="jni_SetIntField")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, obj, fid
      integer(c_int32_t), value :: val
    end subroutine

    subroutine jni_SetLongField(env, obj, fid, val) &
        bind(C, name="jni_SetLongField")
      import :: c_ptr, c_int64_t
      type(c_ptr), value     :: env, obj, fid
      integer(c_int64_t), value :: val
    end subroutine

    subroutine jni_SetFloatField(env, obj, fid, val) &
        bind(C, name="jni_SetFloatField")
      import :: c_ptr, c_float
      type(c_ptr), value :: env, obj, fid
      real(c_float), value :: val
    end subroutine

    subroutine jni_SetDoubleField(env, obj, fid, val) &
        bind(C, name="jni_SetDoubleField")
      import :: c_ptr, c_double
      type(c_ptr), value :: env, obj, fid
      real(c_double), value :: val
    end subroutine

    ! --- Static Field Getters ---
    function jni_GetStaticObjectField(env, clazz, fid) &
        bind(C, name="jni_GetStaticObjectField") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, clazz, fid
      type(c_ptr)        :: r
    end function

    function jni_GetStaticBooleanField(env, clazz, fid) &
        bind(C, name="jni_GetStaticBooleanField") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int8_t)  :: r
    end function

    function jni_GetStaticByteField(env, clazz, fid) &
        bind(C, name="jni_GetStaticByteField") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int8_t)  :: r
    end function

    function jni_GetStaticCharField(env, clazz, fid) &
        bind(C, name="jni_GetStaticCharField") result(r)
      import :: c_ptr, c_int16_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int16_t) :: r
    end function

    function jni_GetStaticShortField(env, clazz, fid) &
        bind(C, name="jni_GetStaticShortField") result(r)
      import :: c_ptr, c_int16_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int16_t) :: r
    end function

    function jni_GetStaticIntField(env, clazz, fid) &
        bind(C, name="jni_GetStaticIntField") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int32_t) :: r
    end function

    function jni_GetStaticLongField(env, clazz, fid) &
        bind(C, name="jni_GetStaticLongField") result(r)
      import :: c_ptr, c_int64_t
      type(c_ptr), value :: env, clazz, fid
      integer(c_int64_t) :: r
    end function

    function jni_GetStaticFloatField(env, clazz, fid) &
        bind(C, name="jni_GetStaticFloatField") result(r)
      import :: c_ptr, c_float
      type(c_ptr), value :: env, clazz, fid
      real(c_float)      :: r
    end function

    function jni_GetStaticDoubleField(env, clazz, fid) &
        bind(C, name="jni_GetStaticDoubleField") result(r)
      import :: c_ptr, c_double
      type(c_ptr), value :: env, clazz, fid
      real(c_double)     :: r
    end function

    ! --- Static Field Setters ---
    subroutine jni_SetStaticObjectField(env, clazz, fid, val) &
        bind(C, name="jni_SetStaticObjectField")
      import :: c_ptr
      type(c_ptr), value :: env, clazz, fid, val
    end subroutine

    subroutine jni_SetStaticBooleanField(env, clazz, fid, val) &
        bind(C, name="jni_SetStaticBooleanField")
      import :: c_ptr, c_int8_t
      type(c_ptr), value    :: env, clazz, fid
      integer(c_int8_t), value :: val
    end subroutine

    subroutine jni_SetStaticIntField(env, clazz, fid, val) &
        bind(C, name="jni_SetStaticIntField")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, clazz, fid
      integer(c_int32_t), value :: val
    end subroutine

    subroutine jni_SetStaticLongField(env, clazz, fid, val) &
        bind(C, name="jni_SetStaticLongField")
      import :: c_ptr, c_int64_t
      type(c_ptr), value     :: env, clazz, fid
      integer(c_int64_t), value :: val
    end subroutine

    subroutine jni_SetStaticDoubleField(env, clazz, fid, val) &
        bind(C, name="jni_SetStaticDoubleField")
      import :: c_ptr, c_double
      type(c_ptr), value :: env, clazz, fid
      real(c_double), value :: val
    end subroutine

    ! --- String Operations ---
    function jni_NewString(env, unicode, len) &
        bind(C, name="jni_NewString") result(r)
      import :: c_ptr, c_int16_t, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int16_t)     :: unicode(*)
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r   ! jstring
    end function

    function jni_GetStringLength(env, str) &
        bind(C, name="jni_GetStringLength") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, str
      integer(c_int32_t) :: r
    end function

    function jni_GetStringChars(env, str, isCopy) &
        bind(C, name="jni_GetStringChars") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, str
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r   ! const jchar*
    end function

    subroutine jni_ReleaseStringChars(env, str, chars) &
        bind(C, name="jni_ReleaseStringChars")
      import :: c_ptr
      type(c_ptr), value :: env, str, chars
    end subroutine

    function jni_NewStringUTF(env, utf) &
        bind(C, name="jni_NewStringUTF") result(r)
      import :: c_ptr, c_char
      type(c_ptr), value :: env
      character(c_char)  :: utf(*)
      type(c_ptr)        :: r
    end function

    function jni_GetStringUTFLength(env, str) &
        bind(C, name="jni_GetStringUTFLength") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, str
      integer(c_int32_t) :: r
    end function

    function jni_GetStringUTFLengthAsLong(env, str) &
        bind(C, name="jni_GetStringUTFLengthAsLong") result(r)
      import :: c_ptr, c_int64_t
      type(c_ptr), value :: env, str
      integer(c_int64_t) :: r
    end function

    function jni_GetStringUTFChars(env, str, isCopy) &
        bind(C, name="jni_GetStringUTFChars") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, str
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r   ! const char*
    end function

    subroutine jni_ReleaseStringUTFChars(env, str, chars) &
        bind(C, name="jni_ReleaseStringUTFChars")
      import :: c_ptr
      type(c_ptr), value :: env, str, chars
    end subroutine

    subroutine jni_GetStringRegion(env, str, start, len, buf) &
        bind(C, name="jni_GetStringRegion")
      import :: c_ptr, c_int32_t, c_int16_t
      type(c_ptr), value     :: env, str
      integer(c_int32_t), value :: start, len
      integer(c_int16_t)     :: buf(*)
    end subroutine

    subroutine jni_GetStringUTFRegion(env, str, start, len, buf) &
        bind(C, name="jni_GetStringUTFRegion")
      import :: c_ptr, c_int32_t, c_char
      type(c_ptr), value     :: env, str
      integer(c_int32_t), value :: start, len
      character(c_char)      :: buf(*)
    end subroutine

    function jni_GetStringCritical(env, str, isCopy) &
        bind(C, name="jni_GetStringCritical") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, str
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r
    end function

    subroutine jni_ReleaseStringCritical(env, str, cstring) &
        bind(C, name="jni_ReleaseStringCritical")
      import :: c_ptr
      type(c_ptr), value :: env, str, cstring
    end subroutine

    ! --- Array Operations ---
    function jni_GetArrayLength(env, array) &
        bind(C, name="jni_GetArrayLength") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, array
      integer(c_int32_t) :: r
    end function

    function jni_NewObjectArray(env, len, clazz, init) &
        bind(C, name="jni_NewObjectArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, clazz, init
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_GetObjectArrayElement(env, array, index) &
        bind(C, name="jni_GetObjectArrayElement") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array
      integer(c_int32_t), value :: index
      type(c_ptr)            :: r
    end function

    subroutine jni_SetObjectArrayElement(env, array, index, val) &
        bind(C, name="jni_SetObjectArrayElement")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array, val
      integer(c_int32_t), value :: index
    end subroutine

    function jni_NewBooleanArray(env, len) &
        bind(C, name="jni_NewBooleanArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewByteArray(env, len) bind(C, name="jni_NewByteArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewCharArray(env, len) bind(C, name="jni_NewCharArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewShortArray(env, len) bind(C, name="jni_NewShortArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewIntArray(env, len) bind(C, name="jni_NewIntArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewLongArray(env, len) bind(C, name="jni_NewLongArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewFloatArray(env, len) bind(C, name="jni_NewFloatArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    function jni_NewDoubleArray(env, len) &
        bind(C, name="jni_NewDoubleArray") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env
      integer(c_int32_t), value :: len
      type(c_ptr)            :: r
    end function

    ! Array element accessors return pointer to C array; use c_f_pointer to bind
    function jni_GetByteArrayElements(env, array, isCopy) &
        bind(C, name="jni_GetByteArrayElements") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, array
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r
    end function

    function jni_GetIntArrayElements(env, array, isCopy) &
        bind(C, name="jni_GetIntArrayElements") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, array
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r
    end function

    function jni_GetDoubleArrayElements(env, array, isCopy) &
        bind(C, name="jni_GetDoubleArrayElements") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, array
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r
    end function

    subroutine jni_ReleaseByteArrayElements(env, array, elems, mode) &
        bind(C, name="jni_ReleaseByteArrayElements")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array, elems
      integer(c_int32_t), value :: mode
    end subroutine

    subroutine jni_ReleaseIntArrayElements(env, array, elems, mode) &
        bind(C, name="jni_ReleaseIntArrayElements")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array, elems
      integer(c_int32_t), value :: mode
    end subroutine

    subroutine jni_ReleaseDoubleArrayElements(env, array, elems, mode) &
        bind(C, name="jni_ReleaseDoubleArrayElements")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array, elems
      integer(c_int32_t), value :: mode
    end subroutine

    subroutine jni_GetIntArrayRegion(env, array, start, len, buf) &
        bind(C, name="jni_GetIntArrayRegion")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array
      integer(c_int32_t), value :: start, len
      integer(c_int32_t)     :: buf(*)
    end subroutine

    subroutine jni_SetIntArrayRegion(env, array, start, len, buf) &
        bind(C, name="jni_SetIntArrayRegion")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array
      integer(c_int32_t), value :: start, len
      integer(c_int32_t)     :: buf(*)
    end subroutine

    subroutine jni_GetDoubleArrayRegion(env, array, start, len, buf) &
        bind(C, name="jni_GetDoubleArrayRegion")
      import :: c_ptr, c_int32_t, c_double
      type(c_ptr), value     :: env, array
      integer(c_int32_t), value :: start, len
      real(c_double)         :: buf(*)
    end subroutine

    subroutine jni_SetDoubleArrayRegion(env, array, start, len, buf) &
        bind(C, name="jni_SetDoubleArrayRegion")
      import :: c_ptr, c_int32_t, c_double
      type(c_ptr), value     :: env, array
      integer(c_int32_t), value :: start, len
      real(c_double)         :: buf(*)
    end subroutine

    function jni_GetPrimitiveArrayCritical(env, array, isCopy) &
        bind(C, name="jni_GetPrimitiveArrayCritical") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, array
      integer(c_int8_t)  :: isCopy
      type(c_ptr)        :: r
    end function

    subroutine jni_ReleasePrimitiveArrayCritical(env, array, carray, mode) &
        bind(C, name="jni_ReleasePrimitiveArrayCritical")
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: env, array, carray
      integer(c_int32_t), value :: mode
    end subroutine

    ! --- Native Method Registration ---
    function jni_RegisterNatives(env, clazz, methods, nMethods) &
        bind(C, name="jni_RegisterNatives") result(r)
      import :: c_ptr, c_int32_t, JNINativeMethod
      type(c_ptr), value     :: env, clazz
      type(JNINativeMethod)  :: methods(*)
      integer(c_int32_t), value :: nMethods
      integer(c_int32_t)     :: r
    end function

    function jni_UnregisterNatives(env, clazz) &
        bind(C, name="jni_UnregisterNatives") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, clazz
      integer(c_int32_t) :: r
    end function

    ! --- Monitor ---
    function jni_MonitorEnter(env, obj) bind(C, name="jni_MonitorEnter") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, obj
      integer(c_int32_t) :: r
    end function

    function jni_MonitorExit(env, obj) bind(C, name="jni_MonitorExit") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env, obj
      integer(c_int32_t) :: r
    end function

    ! --- JavaVM access ---
    function jni_GetJavaVM(env, vm) bind(C, name="jni_GetJavaVM") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: env
      type(c_ptr)        :: vm   ! JavaVM** (out)
      integer(c_int32_t) :: r
    end function

    ! --- Direct Buffer ---
    function jni_NewDirectByteBuffer(env, address, capacity) &
        bind(C, name="jni_NewDirectByteBuffer") result(r)
      import :: c_ptr, c_int64_t
      type(c_ptr), value     :: env, address
      integer(c_int64_t), value :: capacity
      type(c_ptr)            :: r
    end function

    function jni_GetDirectBufferAddress(env, buf) &
        bind(C, name="jni_GetDirectBufferAddress") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, buf
      type(c_ptr)        :: r
    end function

    function jni_GetDirectBufferCapacity(env, buf) &
        bind(C, name="jni_GetDirectBufferCapacity") result(r)
      import :: c_ptr, c_int64_t
      type(c_ptr), value :: env, buf
      integer(c_int64_t) :: r
    end function

    ! --- Reflection ---
    function jni_FromReflectedMethod(env, method) &
        bind(C, name="jni_FromReflectedMethod") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, method
      type(c_ptr)        :: r   ! jmethodID
    end function

    function jni_FromReflectedField(env, field) &
        bind(C, name="jni_FromReflectedField") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, field
      type(c_ptr)        :: r   ! jfieldID
    end function

    function jni_ToReflectedMethod(env, cls, methodID, isStatic) &
        bind(C, name="jni_ToReflectedMethod") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value    :: env, cls, methodID
      integer(c_int8_t), value :: isStatic
      type(c_ptr)           :: r
    end function

    function jni_ToReflectedField(env, cls, fieldID, isStatic) &
        bind(C, name="jni_ToReflectedField") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value    :: env, cls, fieldID
      integer(c_int8_t), value :: isStatic
      type(c_ptr)           :: r
    end function

    ! --- Module (JNI 1.9+) ---
    function jni_GetModule(env, clazz) bind(C, name="jni_GetModule") result(r)
      import :: c_ptr
      type(c_ptr), value :: env, clazz
      type(c_ptr)        :: r
    end function

    ! --- Virtual Threads (JNI 19+) ---
    function jni_IsVirtualThread(env, obj) &
        bind(C, name="jni_IsVirtualThread") result(r)
      import :: c_ptr, c_int8_t
      type(c_ptr), value :: env, obj
      integer(c_int8_t)  :: r
    end function

    ! --- JavaVM Invocation Interface ---
    function jvm_DestroyJavaVM(vm) bind(C, name="jvm_DestroyJavaVM") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: vm
      integer(c_int32_t) :: r
    end function

    function jvm_AttachCurrentThread(vm, penv, args) &
        bind(C, name="jvm_AttachCurrentThread") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: vm, args
      type(c_ptr)        :: penv   ! JNIEnv** (out)
      integer(c_int32_t) :: r
    end function

    function jvm_DetachCurrentThread(vm) &
        bind(C, name="jvm_DetachCurrentThread") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: vm
      integer(c_int32_t) :: r
    end function

    function jvm_GetEnv(vm, penv, version) &
        bind(C, name="jvm_GetEnv") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value     :: vm
      type(c_ptr)            :: penv   ! void** (out)
      integer(c_int32_t), value :: version
      integer(c_int32_t)     :: r
    end function

    function jvm_AttachCurrentThreadAsDaemon(vm, penv, args) &
        bind(C, name="jvm_AttachCurrentThreadAsDaemon") result(r)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: vm, args
      type(c_ptr)        :: penv
      integer(c_int32_t) :: r
    end function

  end interface

end module jni_env
