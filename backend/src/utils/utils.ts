import { isNumberString, isPositive } from "class-validator";
import { ListObject, ListQuery } from "./utils.types";

interface ListQueryRet {
	data : ListObject | null;
	error : string | null;
}

export const validateListquery = (query : ListQuery, keys : string[]) : ListQueryRet => {

	// validate pagination
	if (!query.page || !query.pageSize)
		return {data : null , error : "Page and pageSize required in query"}

	if (!isNumberString(query.page) || !isNumberString(query.pageSize))
		return {data : null , error : "invalid page or pagesize"}

	if (!isPositive(parseInt(query.page, 10)) || !isPositive(parseInt(query.pageSize, 10)))
		return {data : null , error : "negative page or pagesize"}

	// validate operator (must be either AND or OR, defaults to AND)
	if (query.operator && (query.operator !== "AND" && query.operator !== "OR"))
		return {data : null , error : "Operator must be AND or OR"}
	if (!query.operator) query.operator = "AND";

	// valid filter by and filter on (num of elements must match)
	const filterOns : string[] = query.filterOn?.split(",")
	const filterBys : string[] = query.filterBy?.split(",")

	if ((filterBys && filterOns) && (filterOns.length !== filterBys.length))
		return {data : null , error : "filterOn and filterBy length must match"}

	if ((filterBys ==undefined) != (filterOns ==undefined))
		return {data : null , error : "filterOn and filterBy must coexist"}
	
	// valid filter on values
	for (let index = 0; filterOns && index < filterOns.length; index++) {
		const filter = filterOns[index];
		if (!keys.find((key)=>key === filter))
			return {data : null , error : "Invalid filterOn"}
	}

	if ((query.sortBy == undefined) != (query.sortOn == undefined))
		return {data : null , error : "sortOn and sortBy must coexist"}

	// valid sort on and sort by values
	if (query.sortOn && !keys.find((key)=>key === query.sortOn))
			return {data : null , error : "Invalid sortOn"}

	// validate sortby values
	if (query.sortBy && (query.sortBy !== "Ascending" && query.sortBy !== "Descending"))
			return {data : null , error : "sortBy must be Ascending/Descending"}

	return {
		data : {
			page : parseInt(query.page, 10),
			pageSize : parseInt(query.pageSize, 10),
			operator : query.operator,
			filterOns,
			filterBys,
			sortOn : query.sortOn,
			sortBy : query.sortBy,
		},
		error : null
	}
}
