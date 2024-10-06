#include <library.h>

#if defined(_WIN32)
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

DLL load_library(const char* path) { return LoadLibraryA(path); }
void* get_proc_address(DLL library, const char* name) {
  return GetProcAddress(library, name);
}

void free_library(DLL library) { FreeLibrary(library); }

#else
#include <dlfcn.h>

DLL load_library(const char* path) { return dlopen(path, RTLD_LAZY); }

void* get_proc_address(DLL library, const char* name) {
  return dlsym(library, name);
}
void free_library(DLL library) { dlclose(library); }

#endif