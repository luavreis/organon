### A Pluto.jl notebook ###
# v0.12.16

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ 0e77075e-3b2b-11eb-3ccc-b5566e9738bf
begin
	using Markdown
	using Luxor
	using Colors
	using PlutoUI
end

# ╔═╡ 473d669e-3b2b-11eb-0cf8-6525810c72ea
f((x,y)) = (cos(y)^5*3^x, x^3+y) .|> p -> mod(p,1);

# ╔═╡ f2cdbc9e-3bc2-11eb-03da-bf1ad0245bb2
g((x,y)) = let a=2.1, b=5.9
	(sin(x*y/b)*y + cos(a*x - y), x + sin(y)/b) .|> p -> mod(p,1)
end;

# ╔═╡ 9b32c812-3b57-11eb-242b-0bb52e31a065
arnold((x,y)) = (2x + y, x + y) .% 1;

# ╔═╡ d7240ae8-3b57-11eb-188b-3f0ee0efecad
function compose(f, n)
    function (x)
        val = x
        for _ in 1:n
            val = f(val)
        end
        return val
    end
end;

# ╔═╡ 1173cd5a-3b5a-11eb-376d-99a5b00641bf
Base.:^(f::Function, n::Integer) = compose(f,n);

# ╔═╡ 4663058e-3b59-11eb-2929-dbaea3ec1c78
dist((x1,x2),(y1,y2)) = sqrt((x1-y1)^2 + (x2-y2)^2);

# ╔═╡ 168b884a-3b59-11eb-1e46-8d67f5ead8f8
function dyndist(x,y,f,n)
	d = dist(x,y)
	for _ in 1:n
		x = f(x)
		y = f(y)
		d = max(d, dist(x,y))
	end
	d
end;

# ╔═╡ 8284ce6c-3b31-11eb-2796-835ee6d006ca
Np = 800

# ╔═╡ 552c91cc-3b5c-11eb-3301-6dd3e136df1d
@bind cut Slider(0:0.01:1)

# ╔═╡ b03b0738-3b5c-11eb-26c3-173861b22912
@bind n Slider(0:1:20)

# ╔═╡ 3f0b2320-3b5e-11eb-24ea-7d0a59c99f3e
p = (.671,.81);

# ╔═╡ e68f1386-3b5e-11eb-0838-d95297ccef28
fun = f;

# ╔═╡ 66ac124e-3b52-11eb-1bea-339a709ddf25
let	s = 800
	Drawing(s, s, "my-drawing.png")
	background("antiquewhite")
	for x in 0:(1/Np):1, y in 0:(1/Np):1
		x = rand() # * .01 + p[1] - .005
		y = rand() # * .01 + p[2] - .005
		let (tx,ty) = (fun^n)((x,y))
			setcolor(HSV(360tx,.5ty,dyndist((x,y),p,fun,n)))
		end
		if dyndist((x,y),p,fun,n) < cut
			setcolor("black")
		end
		circle(Point(s .* (x, y)), sqrt(s)*0.026, :fill) # .+ (.005,.005) .- p
	end
	setcolor("red")
	circle(s .* Point(p), 5, :fill) # Point(.5,.5)
	finish()
	preview()
end

# ╔═╡ Cell order:
# ╠═0e77075e-3b2b-11eb-3ccc-b5566e9738bf
# ╠═473d669e-3b2b-11eb-0cf8-6525810c72ea
# ╠═f2cdbc9e-3bc2-11eb-03da-bf1ad0245bb2
# ╠═9b32c812-3b57-11eb-242b-0bb52e31a065
# ╠═d7240ae8-3b57-11eb-188b-3f0ee0efecad
# ╠═1173cd5a-3b5a-11eb-376d-99a5b00641bf
# ╠═4663058e-3b59-11eb-2929-dbaea3ec1c78
# ╠═168b884a-3b59-11eb-1e46-8d67f5ead8f8
# ╠═8284ce6c-3b31-11eb-2796-835ee6d006ca
# ╠═552c91cc-3b5c-11eb-3301-6dd3e136df1d
# ╠═b03b0738-3b5c-11eb-26c3-173861b22912
# ╠═3f0b2320-3b5e-11eb-24ea-7d0a59c99f3e
# ╠═e68f1386-3b5e-11eb-0838-d95297ccef28
# ╠═66ac124e-3b52-11eb-1bea-339a709ddf25
