function process(msg)
	return msg
end

test_table = {}
function test_table.process(msg)
	return "test_table:"..msg
end

function call_erlang(base)
	local a = erlang.apply(erlang.nifenv, 'lists', 'sum', {{base, 2, 3}})
	local b = erlang.apply(erlang.nifenv, 'lists', 'sum', {{a, 4, 5}})
	return b + 1
end
