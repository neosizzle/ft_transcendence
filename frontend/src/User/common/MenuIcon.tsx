import React, { FunctionComponent } from "react";

interface MenuIconProps {
	children?: React.ReactNode;
	caption?: string;
	active?: boolean;
	onClick?: React.MouseEventHandler<HTMLDivElement>;
	className?: string;
}

const MenuIcon: FunctionComponent<MenuIconProps> = ({children, caption, active, onClick, className}) => {
	return ( 
		<div
		className={`
		${className}
		${active ? "bg-gray-200" : null}
		p-3
		flex
		flex-col
		justify-center
		items-center
		cursor-pointer
		hover:${active ? "bg-gray-300" : "bg-gray-100" }
		`}
		onClick = {onClick}
		>
			{children}
			<div
			className="
			text-sm
			"
			>
				{caption}
			</div>
		</div>
	);
}
 
export default MenuIcon;