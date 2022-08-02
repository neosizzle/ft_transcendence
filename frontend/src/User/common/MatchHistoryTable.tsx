import React, { FunctionComponent } from "react";

// interface MatchHistoryTableProps {
	
// }
 
const TableRow = () => {
	return <tr>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-[#F3F6FF]
	border-b border-l border-[#E8E8E8]
	"
	>
	1/1/1111
	</td>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-white
	border-b border-[#E8E8E8]
	"
	>
	MEIMEI_HUNTER1234
	</td>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-[#F3F6FF]
	border-b border-[#E8E8E8]
	"
	>
	Classic
	</td>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-white
	border-b border-[#E8E8E8]
	"
	>
	69-420
	</td>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-[#F3F6FF]
	border-b border-[#E8E8E8]
	"
	>
	MEIMEI_HUNTER1234
	</td>
	<td
	className="
	text-center text-dark
	text-base
	py-5
	px-2
	bg-white
	border-b border-r border-[#E8E8E8]
	"
	>
	<a
		href=""
		className="
		border border-primary
		py-2
		px-6
		text-primary
		inline-block
		rounded
		hover:bg-primary hover:text-white
		"
		>
	Delete
	</a>
	</td>
</tr>
}

const MatchHistoryTable: FunctionComponent = () => {
	return ( 
		<div
		className = "flex justify-center pt-4"
		>
			<div
			className="w-5/6"
			>
				<div
				className="
				pb-1
				text-lg
				"
				>
					<h1>Match history</h1>
				</div>
				{/* <!-- ====== Table Section Start --> */}
				<section className="bg-white pb-20 sm:pb-0">
				<div className="container">
					<div className="flex flex-wrap -mx-4">
						<div className="w-full px-4">
							<div className="max-w-full max-h-screen overflow-auto">
							<table className="table-auto w-full">
								<thead>
									<tr className="bg-primary text-center">
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										border-l border-transparent
										bg-slate-400
										"
										>
										Match Date
										</th>
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										bg-slate-400
										"
										>
										Opponent
										</th>
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										bg-slate-400
										"
										>
										Gamemode
										</th>
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										bg-slate-400
										"
										>
										Score
										</th>
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										bg-slate-400
										"
										>
										Winner
										</th>
										<th
										className="
										w-1/6
										min-w-[160px]
										text-lg
										font-semibold
										text-white
										py-4
										lg:py-7
										px-3
										lg:px-4
										border-r border-transparent
										bg-slate-400
										"
										>
										Action
										</th>
									</tr>
								</thead>
								<tbody>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
									<TableRow/>
								</tbody>
							</table>
							</div>
						</div>
					</div>
				</div>
				</section>
				{/* <!-- ====== Table Section End --> */}
			</div>
		</div>
	);
}
 
export default MatchHistoryTable;