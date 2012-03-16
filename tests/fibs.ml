const     = \x y. x;
flip      = \f x y. f y x;
undefined = fix g. g;

zero   = \f x. x;
succ   = \n f x. f (n f x);
one    = succ zero;
two    = succ one;
three  = succ two;
four   = succ three;
plus   = \m n f x. m f (n f x);

true  = \x y . x;
false = \x y . y;
and   = \b1 b2 . b1 b2 false;
or    = \b1 b2 . b1 true b2;

iszero = \n . n (const false) true;

(\f x y . f y x) (\x y . x);