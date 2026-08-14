#include "../include/lld-17/Common/CommonLinkerContext.h"
#include "../include/lld-17/Common/Driver.h"

#include <cstdlib>
#include <iostream>
#include <mutex>

LLD_HAS_DRIVER(elf)
LLD_HAS_DRIVER(coff)
LLD_HAS_DRIVER(macho)
LLD_HAS_DRIVER(wasm)

std::mutex concurrencyMutex;

const char *alloc_string(const std::string &str) {
    size_t size = str.length();

    if (size > 0) {
        char *strPtr = reinterpret_cast<char *>(malloc(size + 1));
        memcpy(strPtr, str.c_str(), size + 1);

        return strPtr;
    }
    
    return nullptr;
}

extern "C" {
    enum LLDFlavor {
        Elf = 0,
        Wasm = 1,
        MachO = 2,
        Coff = 3,
    };

    struct LLDInvokeResult {
        bool success;
        const char *messages;
    };

    void lld_free(LLDInvokeResult *result) {
        if (result->messages) free(reinterpret_cast<void *>(const_cast<char *>(result->messages)));
    }
}

auto getLinkerForFlavor(LLDFlavor flavor) {
    switch (flavor) {
        case Wasm:
            return lld::wasm::link;
        case MachO:
            return lld::macho::link;
        case Coff:
            return lld::coff::link;
        case Elf:
        default:
            return lld::elf::link;
    }
}

extern "C" {

    LLDInvokeResult link_with_lld(LLDFlavor flavor, int argc, const char *const *argv) {
        LLDInvokeResult result;

        std::string outputString, errorString;

        llvm::raw_string_ostream outputStream(outputString);
        llvm::raw_string_ostream errorStream(errorString);
        
        std::unique_lock<std::mutex> lock(concurrencyMutex);

        auto link_fn = ::getLinkerForFlavor(flavor);

        std::vector<const char *> args(argv, argv + argc);

        if (flavor == Coff) {
            args.insert(args.begin(), "lld.exe");
        } else {
            args.insert(args.begin(), "lld");
        }

        result.success = link_fn(args, outputStream, errorStream, false, false);
        result.messages = ::alloc_string(errorStream.str() + outputStream.str());

        lld::CommonLinkerContext::destroy();

        return result;
    }

}
