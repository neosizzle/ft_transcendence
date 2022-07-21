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
		${active ? "bg-gray-100" : null}
		p-3
		flex
		flex-col
		justify-center
		items-center
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