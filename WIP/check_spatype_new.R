setMethod("check_spatype",
    signature(object = "character"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
setMethod("check_spatype",
    signature(object = "Raster"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
setMethod("check_spatype",
    signature(object = "Spatial"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
setMethod("check_spatype",
    signature(object = "sf"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
setMethod("check_spatype",
    signature(object = "sfc"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
setMethod("check_spatype",
    signature(object = "default"),
    function (object) 
    {
        stop("need a definition for the method here")
    }
)
