const     = \x y . x;
flip      = \f x y . f y x;
undefined = fix g. g;

zero   = \f x . x;
succ   = \n f x . f (n f x);
one    = succ zero;
two    = succ one;
three  = succ two;
four   = succ three;
plus   = \m n f x . m f (n f x);

true  = const;
false = flip const;

iszero = \n . n (const false) true;

plus zero zero;
