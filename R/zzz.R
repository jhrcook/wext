
# clean up after C++ code
.onUnload <- function (libpath) {
    library.dynam.unload("wext", libpath)
}
