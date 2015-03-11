
biseccion f a b error limite | f a == 0 = a
                        	   | f b == 0 = b
                         	   | otherwise = biseccionIterativo 0 f a b 0 error limite

biseccionIterativo n f a b x error limite | (error > abs (b - a)) || (n > limite) = x
                                          | mult > 0 = biseccionIterativo (n + 1) f c b c error limite
                                          | mult < 0 = biseccionIterativo (n + 1) f a c c error limite
                                          | otherwise = c
  where mult = f a * f c
        c = (a + b) / 2
