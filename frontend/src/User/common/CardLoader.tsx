import React, { FunctionComponent } from "react";

const CardLoader: FunctionComponent = () => {
	
	return ( 
		<div className =
		"w-full flex justify-center mt-2">
			<div className="w-10/12 bg-white rounded-lg border border-gray-200 shadow-md p-3">
				<div className="grid grid-cols-12 gap-4 animate-pulse">

					{/* Avatar */}
					<div className="col-span-3 flex justify-center items-center">
						{/* <img className="
							inline-block
							h-10
							w-10
							sm:h-16
							sm:w-16
							rounded-full
							ring-1
							ring-white
							mt-3
							mb-1
							" src={userToDisplay?.avatar || '/assets/default-pp.webp'} alt=""/> */}
							<div className="
							inline-block
							h-10
							w-10
							sm:h-16
							sm:w-16
							rounded-full
							ring-1
							ring-white
							bg-gray-300
							"/>
					</div>

					{/* user info */}
					<div className="col-span-6">
						<div>
							<div className="w-36 bg-gray-300 h-6 rounded-md"></div>
						</div>

						<div>
						<div className="w-20 bg-gray-300 h-4 rounded-md mt-2"></div>
						</div>

					</div>

				</div>
			</div>
		</div>
	);
}
 
export default CardLoader;