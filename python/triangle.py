[(x,y,z) for x in range(1,11) for y in range(1,11) for z in range(1,11) if x+y+z == 24 if x**2 + y**2 == z**2 if x<=z if y<=z]
