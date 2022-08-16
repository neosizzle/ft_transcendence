import React, { FunctionComponent, useState } from "react";
import { Sort } from "../Search";

interface SortMenuProps {
  sort: Sort;
  setSort: React.Dispatch<React.SetStateAction<Sort>>;
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

const SortMenu: FunctionComponent<SortMenuProps> = ({ sort, setSort }) => {
  const [currSortOn, setCurrSortOn] = useState<string>(sort.sortOn); // current sorton for ui selection
  const [currSortBy, setCurrSortBy] = useState<string>(sort.sortBy); // curr sortby for ui selection

  const handleSortOnSelect = (value: string) => {
    const sortCpy: Sort = { ...sort };
    sortCpy.sortOn = value;
    setCurrSortOn(value);
    setSort(sortCpy);
  };

  const handleSortBySelect = (value: string) => {
    const sortCpy: Sort = { ...sort };
    sortCpy.sortBy = value;
    setCurrSortBy(value);
    setSort(sortCpy);
  };

  return (
    <div className="p-4">
      <div className="text-lg pb-2 ">Sort On</div>
      <div className="grid grid-cols-2 gap-4">
        <MenuBtn
          handleClick={() => handleSortOnSelect("username")}
          active={currSortOn === "username"}
        >
          Username
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortOnSelect("intraName")}
          active={currSortOn === "intraName"}
        >
          Intra Name
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortOnSelect("email")}
          active={currSortOn === "email"}
        >
          Email
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortOnSelect("level")}
          active={currSortOn === "level"}
        >
          Level
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortOnSelect("createdAt")}
          active={currSortOn === "createdAt"}
        >
          Created At
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortOnSelect("status")}
          active={currSortOn === "status"}
        >
          Status
        </MenuBtn>
      </div>

      <div className="text-lg py-2 ">Sort By</div>
      <div className="grid grid-cols-2 gap-4">
        <MenuBtn
          handleClick={() => handleSortBySelect("Ascending")}
          active={currSortBy === "Ascending"}
        >
          Ascending
        </MenuBtn>
        <MenuBtn
          handleClick={() => handleSortBySelect("Descending")}
          active={currSortBy === "Descending"}
        >
          Descending
        </MenuBtn>
      </div>
    </div>
  );
};

export default SortMenu;
