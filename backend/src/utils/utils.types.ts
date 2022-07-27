export interface ListObject {
	page : number;
	pageSize : number;
	operator : string;
	filterOns : string[];
	filterBys : string[];
	sortOn : string;
	sortBy : string;
}

export interface ListQuery {
	operator ? : string,
	filterOn ? : string,
	filterBy ? : string,
	sortOn ? : string,
	sortBy ? : string,
	pageSize ? : string,
	page ? : string,
}
