
function vertical_reflection(pattern)
    _, cols = size(pattern)
    for col in 1:(cols-1)
        issymmetric = true
        for offset in 1:(min(col, cols - col))
            if pattern[:, col-offset+1] != pattern[:, col+offset]
                issymmetric = false
                break
            end
        end
        if issymmetric
            return col
        end
    end
end

function vertical_smudge_reflection(pattern)
    rows, cols = size(pattern)
    for col in 1:(cols-1)
        smudges = 0
        for offset in 1:(min(col, cols - col))
            smudges += count(
                row -> (pattern[row, col-offset+1] != pattern[row, col+offset]),
                1:rows
            )
        end
        if smudges == 1
            return col
        end
    end
end

function point_of_incidence(pattern::Matrix{Char}, pt2::Bool)::Int
    fun = pt2 ? vertical_smudge_reflection : vertical_reflection
    v_reflection = fun(pattern)
    if !isnothing(v_reflection)
        return v_reflection
    end
    return 100 * fun(permutedims(pattern, (2, 1)))
end

function point_of_incidence(lines::String, pt2::Bool)::Int
    return mapreduce(
        pattern -> point_of_incidence(pattern, pt2),
        +,
        map(
            pattern -> stack(filter(!isempty, map(collect, split(pattern, '\n'))); dims=1),
            eachsplit(lines, "\n\n")
        )
    )
end

# pt1
@time println("Part 1: $(point_of_incidence(read("src/13/input.txt", String), false))")

# pt2
@time println("Part 2: $(point_of_incidence(read("src/13/input.txt", String ), true))")
