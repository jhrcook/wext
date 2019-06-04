
# clean up after C++ code
.onUnload <- function (libpath) {
    library.dynam.unload("mypackage", libpath)
}
