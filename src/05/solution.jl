const r_number = r"(\d+)"
const r_seed_range = r"(?<from>\d+) (?<length>\d+)"
const r_mapping = r"(?<dest>\d+) (?<source>\d+) (?<length>\d+)"

struct Id_mapping
    source_range::AbstractRange
    distance::Int
end

function get_id_mappings(mapping_rules_segment::AbstractString)::Vector{Id_mapping}
    return [
        let
            dest = parse(Int, mapping_match["dest"])
            source = parse(Int, mapping_match["source"])
            length = parse(Int, mapping_match["length"])

            Id_mapping(source:(source+length-1), dest - source)
        end

        for mapping_match in eachmatch(r_mapping, mapping_rules_segment)
    ]
end

function if_you_give_a_seed_a_fertilizer_pt1(path::String)::Int
    text_segments = split(read(path, String), "\n\n")

    seeds = Dict{Int,Int}()
    for match in eachmatch(r_number, text_segments[1])
        id = parse(Int, match.match)
        seeds[id] = id
    end
    for text_segment in text_segments[2:end]
        id_mappings = get_id_mappings(text_segment)
        for seed_id in keys(seeds)
            for id_mapping in id_mappings
                if seeds[seed_id] in id_mapping.source_range
                    seeds[seed_id] += id_mapping.distance
                    break
                end
            end
        end
    end
    return reduce(min, values(seeds))
end

function get_seed_ranges(line::AbstractString)::Vector{AbstractRange}
    [
        let from = parse(Int, match["from"]), length = parse(Int, match["length"])
            from:(from+length-1)
        end
        for match in eachmatch(r_seed_range, line)
    ]
end

function range_diff(lhs::AbstractRange, rhs::AbstractRange)::Tuple{AbstractRange,AbstractRange,AbstractRange}
    intersection = intersect(lhs, rhs)
    return (lhs.start:(intersection.start-1), intersection, (intersection.stop+1):lhs.stop)
end

function perform_mapping(input_ranges::Vector{AbstractRange}, mapping_rules_segment::AbstractString)::Vector{AbstractRange}
    id_mappings = get_id_mappings(mapping_rules_segment)
    output_ranges = Vector{AbstractRange}()

    input_ranges = copy(input_ranges)

    while !isempty(input_ranges)
        input_range = pop!(input_ranges)
        if isempty(input_range)
            continue
        end
        match_found = false
        for id_mapping in id_mappings
            lhs, intersection, rhs = range_diff(input_range, id_mapping.source_range)
            if !isempty(intersection)
                match_found = true
                push!(output_ranges, intersection .+ id_mapping.distance)
                push!(input_ranges, lhs)
                push!(input_ranges, rhs)
                break
            end
        end
        if !match_found
            push!(output_ranges, input_range)
        end
    end
    return output_ranges
end

function if_you_give_a_seed_a_fertilizer_pt2(path::String)::Int
    text_segments = split(read(path, String), "\n\n")
    id_ranges = get_seed_ranges(text_segments[1])
    for mapping_rule_segment in text_segments[2:end]
        id_ranges = perform_mapping(id_ranges, mapping_rule_segment)
    end
    return mapreduce(
        range -> range.start,
        min,
        id_ranges
    )
end

# pt1
@time println("Part 1: $(if_you_give_a_seed_a_fertilizer_pt1("src/05/input.txt"))")

# pt2
@time println("Part 2: $(if_you_give_a_seed_a_fertilizer_pt2("src/05/input.txt"))")