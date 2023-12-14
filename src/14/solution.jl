const num_cycles = 1000000000

@enum Direction north south east west

get_load(column::AbstractVector{Char})::Int =
    reduce(+, findall(c -> c == 'O', reverse(column)))

get_load(dish::Matrix{Char})::Int = mapreduce(get_load, +, eachcol(dish))

function step!(line::AbstractArray, rev::Bool)
    cube_rock_idx = findfirst(c -> c == '#', line)
    if isnothing(cube_rock_idx)
        sort!(line; rev=rev)
        return
    end
    @views begin
        sort!(line[1:(cube_rock_idx-1)]; rev=rev)
        step!(line[(cube_rock_idx+1):end], rev)
    end
end

function step!(dish::Matrix{Char}, direction::Union{Direction,Nothing}=nothing)
    if isnothing(direction) || direction == north
        for col in eachcol(dish)
            step!(col, true)
        end
    end
    if isnothing(direction) || direction == west
        for row in eachrow(dish)
            step!(row, true)
        end
    end
    if isnothing(direction) || direction == south
        for col in eachcol(dish)
            step!(col, false)
        end
    end
    if isnothing(direction) || direction == east
        for row in eachrow(dish)
            step!(row, false)
        end
    end
end

function parabolic_reflector_dish_pt1(dish::Matrix{Char})::Int
    dish = copy(dish)
    step!(dish, north)
    return get_load(dish)
end

function parabolic_reflector_dish_pt2(dish::Matrix{Char})::Int
    dish = copy(dish)
    dishes = Dict{Matrix{Char},Int}()
    i = 0
    while !haskey(dishes, dish)
        dishes[copy(dish)] = i
        step!(dish)
        i += 1
    end
    loop_length = i - dishes[dish]
    remaining_cycles = (num_cycles - i) % loop_length
    for _ in 1:(remaining_cycles)
        step!(dish)
    end
    return get_load(dish)
end


dish = stack(filter(!isempty, map(collect, split(read("src/14/input.txt", String), '\n'))); dims=1)

# pt1
@time println("Part 1: $(parabolic_reflector_dish_pt1(dish))")

# pt2
@time println("Part 2: $(parabolic_reflector_dish_pt2(dish))")
