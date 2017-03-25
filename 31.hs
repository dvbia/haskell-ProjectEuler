currency = reverse [200, 100, 50, 20, 10, 5, 2, 1]


exchange x curr_list
	| x < 0 = 0
	| x == 0 = 1
	| null curr_list = 0
	| otherwise = exchange x (tail curr_list) + 
			exchange (x-(head curr_list)) curr_list +
			exchange (x-(head curr_list)) (tail curr_list)
