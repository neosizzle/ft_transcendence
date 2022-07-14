import React, { FunctionComponent, useState } from "react";

interface PaginationProps {
	setCurrPage: React.Dispatch<React.SetStateAction<number>>;
	currPage : number;
	totalElements: number;
	pageSize : number;
}

const Pagination: FunctionComponent<PaginationProps> = ({setCurrPage, currPage, totalElements, pageSize}) => {
	const [totalPages, setTotalPages] = useState<number>(Math.ceil(totalElements / pageSize));
	return ( 
		<div
		className="
		w-full
		flex
		justify-center
		mt-2
		"
		>
			<div
			className="
			w-4/5
			"
			>
				{totalPages}
			</div>
		</div>
	);
}
 
export default Pagination;