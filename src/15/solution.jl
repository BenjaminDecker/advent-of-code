hash(acc::Int, c::Char)::Int = ((acc + Int(c)) * 17) % 256

function lens_library_pt1(initialization_sequence::Vector{Char})::Int
    return reduce(hash, initialization_sequence, init=0)
end

function lens_library_pt1(initialization_sequence::Vector{Vector{Char}})::Int
    return mapreduce(lens_library_pt1, +, initialization_sequence)
end

mutable struct Lens
    label::Vector{Char}
    focal_length::Int
end

function lens_library_pt2(initialization_sequence::Vector{Vector{Char}})::Int
    boxes = [Vector{Lens}() for _ in 1:256]
    for step in initialization_sequence
        operator_index = findfirst(c -> c == '-' || c == '=', step)
        label = step[1:operator_index-1]
        box_idx = lens_library_pt1(label)
        box = boxes[box_idx+1]
        lens_idx = findfirst(lens -> lens.label == label, box)
        if step[operator_index] == '-'
            if !isnothing(lens_idx)
                deleteat!(box, lens_idx)
            end
        else
            focal_length = parse(Int, String(step[operator_index+1:end]))
            if !isnothing(lens_idx)
                box[lens_idx].focal_length = focal_length
            else
                push!(box, Lens(label, focal_length))
            end
        end
    end

    return mapreduce(
        +,
        enumerate(boxes)
    ) do (box_idx, box)
        mapreduce(
            +,
            enumerate(box);
            init=0
        ) do (lens_idx, lens)
            box_idx * lens_idx * lens.focal_length
        end
    end
end


input = map(collect, filter(!contains('\n'), split(read("src/15/input.txt", String), ',')))

# pt1
@time println("Part 1: $(lens_library_pt1(input))")

# pt2
@time println("Part 2: $(lens_library_pt2(input))")
