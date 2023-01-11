package DSL

inline def booleanToString(inline x: Any): String                        = ${ booleanToStringImpl('x) }
inline def modelPrinter[R](inline model: Model[R, Error]): String = ${ modelPrinterImpl[R]('{ model }) }