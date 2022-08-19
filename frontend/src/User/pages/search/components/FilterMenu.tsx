import React, { FunctionComponent, useState } from "react";
import { Filter } from "../Search";

interface FilterMenuProps {
  filter: Filter;
  setFilter: React.Dispatch<React.SetStateAction<Filter>>;
}

interface MenuBtnProps {
  handleClick: () => void;
  active: boolean;
  children: React.ReactNode;
}

const MenuBtn: FunctionComponent<MenuBtnProps> = ({
  handleClick,
  active,
  children,
}) => {
  return (
    <button
      className={`${
        active
          ? "text-white bg-gray-600 hover:bg-gray-700"
          : "text-black hover:bg-gray-300"
      } w-full block rounded px-4 py-2 shadow-md`}
      onClick={handleClick}
    >
      {children}
    </button>
  );
};

const FilterMenu: FunctionComponent<FilterMenuProps> = ({
  filter,
  setFilter,
}) => {
  const [currStatus, setCurrStatus] = useState<string>(filter.status || ""); // current status for ui selection
  const [currFilter, setCurrFilter] = useState<string>(
    typeof filter.email == "string"
      ? "email"
      : typeof filter.intraName == "string"
      ? "intraName"
      : "username"
  ); // curr filter for ui selection

  const handleSearchSelect = (value: string) => {
    const filterCpy: Filter = { ...filter };

    Object.keys(filterCpy).forEach((key: string) => {
      if (key === "status") return;
      delete filterCpy[key];
    });
    filterCpy[value] = "";
    setCurrFilter(value);
    setFilter(filterCpy);
  };

  const handleStatusSelect = (value: string) => {
    const filterCpy: Filter = { ...filter };

    // sets to undefined if selected again
    if (filterCpy.status === value) {
      delete filterCpy.status;
      setCurrStatus("");
    } else {
      filterCpy.status = value;
      setCurrStatus(value);
    }
    setFilter(filterCpy);
  };

  return (
    <div className="p-4">
      <div className="text-lg pb-2 ">Search by</div>
      <div className="grid grid-cols-2 gap-4">
        <MenuBtn
          handleClick={() => handleSearchSelect("username")}
          active={currFilter === "username"}
        >
          Username
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSearchSelect("intraName")}
          active={currFilter === "intraName"}
        >
          Intra Name
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSearchSelect("email")}
          active={currFilter === "email"}
        >
          Email
        </MenuBtn>
      </div>

      <div className="text-lg py-2 ">Filter by</div>
      <MenuBtn
        handleClick={() => handleStatusSelect("LOGGEDIN")}
        active={currStatus === "LOGGEDIN"}
      >
        ONLINE
      </MenuBtn>
      <MenuBtn
        handleClick={() => handleStatusSelect("INGAME")}
        active={currStatus === "INGAME"}
      >
        INGAME
      </MenuBtn>
      <MenuBtn
        handleClick={() => handleStatusSelect("OFFLINE")}
        active={currStatus === "OFFLINE"}
      >
        OFFLINE
      </MenuBtn>
    </div>
  );
};

export default FilterMenu;
