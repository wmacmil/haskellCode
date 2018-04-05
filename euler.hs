-- the solver
dsolveBy _ _ [] _ = error "empty solution interval"
dsolveBy method f mesh x0 = zip mesh results
  where results = scanl (method f) x0 intervals
        intervals = zip mesh (tail mesh)


-- 1-st order Euler

let euler f x (t1,t2) = x + (t2 - t1) * f t1 x

-- 2-nd order Runge-Kutta
rk2 f x (t1,t2) = x + h * f (t1 + h/2) (x + h/2*f t1 x)
  where h = t2 - t1

-- 4-th order Runge-Kutta
rk4 f x (t1,t2) = x + h/6 * (k1 + 2*k2 + 2*k3 + k4)
  where k1 = f t1 x
        k2 = f (t1 + h/2) (x + h/2*k1)
        k3 = f (t1 + h/2) (x + h/2*k2)
        k4 = f (t1 + h) (x + h*k3)
        h = t2 - t1
