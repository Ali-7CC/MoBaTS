package DSL

inline def booleanToString(inline cond: Boolean): String          = ${ booleanToStringImpl('cond) }
inline def modelPrinter[R](inline model: Model[R, Error]): String = ${ modelPrinterImpl[R]('{ model }) }
