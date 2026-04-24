#include <jni.h>
#include <stdint.h>

// Version and Class Ops
jint jni_GetVersion(JNIEnv *env) {
    return (*env)->GetVersion(env);
}

jclass jni_FindClass(JNIEnv *env, const char *name) {
    return (*env)->FindClass(env, name);
}

// This allows Fortran to grab a pointer to Java memory
jsize jni_GetArrayLength(JNIEnv *env, jarray array) {
    return (*env)->GetArrayLength(env, array);
}

jint* jni_GetIntArrayElements(JNIEnv *env, jintArray array, jboolean *isCopy) {
    return (*env)->GetIntArrayElements(env, array, isCopy);
}

void jni_ReleaseIntArrayElements(JNIEnv *env, jintArray array, jint *elems, jint mode) {
    (*env)->ReleaseIntArrayElements(env, array, elems, mode);
}

jmethodID jni_GetMethodID(JNIEnv *env, jclass clazz, const char *name, const char *sig) {
    return (*env)->GetMethodID(env, clazz, name, sig);
}

const char* jni_GetStringUTFChars(JNIEnv *env, jstring str, jboolean *isCopy) {
    return (*env)->GetStringUTFChars(env, str, isCopy);
}

void jni_ReleaseStringUTFChars(JNIEnv *env, jstring str, const char *chars) {
    (*env)->ReleaseStringUTFChars(env, str, chars);
}

jint jvm_GetEnv(JavaVM *vm, void **env, jint version) {
    return (*vm)->GetEnv(vm, env, version);
}

jint jvm_AttachCurrentThread(JavaVM *vm, void **env, void *args) {
    return (*vm)->AttachCurrentThread(vm, env, args);
}

jint jvm_DetachCurrentThread(JavaVM *vm) {
    return (*vm)->DetachCurrentThread(vm);
}
