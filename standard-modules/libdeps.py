from typing import List

from cffi import FFI
ffi = FFI()
ffi.cdef('''
    typedef struct {
        const char* name;
        const char* oid;
    } Module;
         
    typedef struct {
        Module module;
        const Module* dependencies;
        uint32_t dependencies_len;
    } Dependencies;
         
    typedef struct {
        const Dependencies* dependencies;
        const char* error;
    } GetModuleDependenciesResult;

    GetModuleDependenciesResult get_module_dependencies(const char* module_source);
''')

libdeps = ffi.dlopen('./libdeps/target/release/libdeps.so')

def module_to_dict(module) -> dict:
    return {
        'name': ffi.string(module.name).decode(),
        'oid': ffi.string(module.oid).decode() if module.oid != ffi.NULL else None,
    }

# TODO: why does SymbolsFromList freak out when the first symbol imported from a module, when a valuereference, AFTER the first module import?
# i.e.:
# IMPORTS
#     OBJECT-TYPE, Counter32, Gauge32, MODULE-IDENTITY, mib-2
#       FROM SNMPv2-SMI
#     displayString, TimeInterval
#       FROM SNMPv2-TC
# displayString breaks it.
# "DisplayString, TimeInterval" fixes it.
# "DisplayString, timeInterval" fixes it.
# Removing the SNMPv2-SMI section but keeping "displayString, TimeInterval" fixes it. 
 
def get_module_dependencies(module_source: str) -> List[dict]:
    module_source_cstr = ffi.new('char[]', module_source.encode())
    res = libdeps.get_module_dependencies(module_source_cstr)
    ffi.release(module_source_cstr)
    if res.error != ffi.NULL:
        raise ValueError(ffi.string(res.error).decode())
    else:
        cdeps = res.dependencies
        deps = []
        print(f'got {cdeps.dependencies_len} deps')
        for i in range(cdeps.dependencies_len):
            deps.append(module_to_dict(cdeps.dependencies[i]))

        # TODO: implement free_result in Rust to prevent memory leakage

        return {
            'module': module_to_dict(cdeps.module),
            'dependencies': deps,
        }
