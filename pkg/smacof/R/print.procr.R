print.procr <- function(x,...)
  {
    cat("\nCall: ")
    print(x$call)
    cat("\nCongruence coefficient:", round(x$congcoef, 3))
    cat("\nAlienation coefficient:", round(x$aliencoef, 3))
    cat("\nCorrelation coefficient:", round(x$r, 3))
    
    
    cat("\n\nRotation matrix:\n")
    print(round(x$rotation, 3))
    cat("\nTranslation vector:",round(x$translation, 3),"\n")
    cat("Dilation factor:",round(x$dilation, 3),"\n")
    cat("\n")
  }
