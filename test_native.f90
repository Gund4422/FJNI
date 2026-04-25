function Java_Bootstrap_addNumbers(env, cls, a, b) &
    bind(C, name="Java_Bootstrap_addNumbers") result(r)
  use iso_c_binding
  use jni
  implicit none
  type(c_ptr), value :: env, cls
  integer(jint), value :: a, b
  integer(jint) :: r
  r = a + b
end function
