import React, { FunctionComponent } from "react";

interface DropdownProps {
  children: React.ReactNode;
  label: string;
  openDropdown?: boolean;
  setOpenDropdown: React.Dispatch<React.SetStateAction<boolean>>;
}

const Dropdown: FunctionComponent<DropdownProps> = ({
  label,
  children,
  openDropdown,
  setOpenDropdown,
}) => {
  return (
    <div>
      <button
        onClick={() => {
          setOpenDropdown(!openDropdown);
        }}
        className="font-medium rounded-lg text-sm px-4 py-2.5 text-center inline-flex items-center"
        type="button"
      >
        {label}{" "}
        <svg
          className="ml-2 w-4 h-4"
          aria-hidden="true"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth="2"
            d="M19 9l-7 7-7-7"
          ></path>
        </svg>
      </button>

      {/* <!-- Dropdown menu --> */}
      {openDropdown ? (
        <div
          id="dropdown"
          className="z-10 w-screen sm:w-max left-0 sm:left-auto bg-white absolute rounded divide-y divide-gray-100 shadow dark:bg-gray-700"
        >
          {children}
        </div>
      ) : null}
    </div>
  );
};

export default Dropdown;
