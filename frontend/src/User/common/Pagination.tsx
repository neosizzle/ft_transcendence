import React, { FunctionComponent, MouseEventHandler, useEffect, useState } from "react";

interface PaginationProps {
	setCurrPage: React.Dispatch<React.SetStateAction<number>>;
	currPage : number;
	totalElements: number;
	pageSize : number;
}

interface PaginationBtnProps {
	children : React.ReactNode;
	onClick: MouseEventHandler;
}

const MAX_INT = 2147483647;
const FORWARD = 0;
const BACKWARD = 1;

const PaginationBtn: FunctionComponent<PaginationBtnProps>  = ({children, onClick}) => {
	return <button onClick={onClick}>
		{children}
	</button>
}

const Pagination: FunctionComponent<PaginationProps> = ({setCurrPage, currPage, totalElements, pageSize}) => {
	const [totalPages, setTotalPages] = useState<number>(Math.ceil(totalElements / pageSize));
	const [displayedPages, setDisplayedPages] = useState<number[]>([...Array(5)].fill(MAX_INT));

	useEffect(() => {
		const newDisplayedPages : number[] = [...Array(5)].fill(MAX_INT);

		[...Array(5)].forEach((e, i)=>{
			if (i < totalPages)
				newDisplayedPages[i] = i + 1;
		})
		// console.log("displaypages ",newDisplayedPages)
		setDisplayedPages(newDisplayedPages);
	}, [])

	/**
	 * Function that handles front / back shifting when the ... button is clicked
	 * @param direction direction of shift
	 */
	const handleShift = (direction : number) => {
		const currDisplayedPages = [...displayedPages];

		currDisplayedPages.forEach((e, i) => currDisplayedPages[i] = direction === FORWARD ? e + 1 : e - 1);
		setDisplayedPages(currDisplayedPages);
		if (direction === FORWARD) setCurrPage(currDisplayedPages[currDisplayedPages.length - 1])
		else setCurrPage(currDisplayedPages[0]);
	}

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
				<PaginationBtn onClick={()=>{{
					if (currPage == 1) return
					if (currPage === displayedPages[0]) return handleShift(BACKWARD);
					setCurrPage(currPage - 1);
				}}}> back </PaginationBtn>
				<br/>
				{displayedPages[0] != MAX_INT && displayedPages[0] > 1? <PaginationBtn onClick={()=>handleShift(BACKWARD)}> ...</PaginationBtn> : null }
				{
					displayedPages.map((e)=>{
						if (e != MAX_INT)
							return <div key = {e}><PaginationBtn onClick={()=>setCurrPage(e)}>{e} {currPage === e ? "active" : null}</PaginationBtn></div>
						return null;
					})
				}
				{displayedPages[displayedPages.length - 1] < totalPages? <PaginationBtn onClick={()=>handleShift(FORWARD)}>...</PaginationBtn> : null }
				<br/>
				<PaginationBtn onClick={()=>{{
					if (currPage == totalPages) return
					if (currPage === displayedPages[displayedPages.length - 1]) return handleShift(FORWARD);
					setCurrPage(currPage + 1);
				}}}> front </PaginationBtn>
				{/* {totalPages}  */}
			</div>
		</div>
	);
}
 
export default Pagination;