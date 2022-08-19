import React, { FunctionComponent, useEffect, useState } from "react";
import Dropdown from "../../../common/Dropdown";
import SearchBar from "../../../common/SearchBar";
import { Filter, Sort } from "../Search";
import FilterMenu from "./FilterMenu";
import SortMenu from "./SortMenu";

interface SearchSectionProps {
  searchLoading: boolean;
  filter: Filter;
  setFilter: React.Dispatch<React.SetStateAction<Filter>>;
  sort: Sort;
  setSort: React.Dispatch<React.SetStateAction<Sort>>;
  setSearchInput: React.Dispatch<React.SetStateAction<string>>;
}

const SearchSection: FunctionComponent<SearchSectionProps> = ({
  searchLoading,
  filter,
  setFilter,
  sort,
  setSort,
  setSearchInput,
}) => {
  const [openFilterDropdown, setOpenFilterDropdown] = useState<boolean>(false); // open filter dropdown
  const [openSortDropdown, setOpenSortDropdown] = useState<boolean>(false); // open sort dropdown

  // only 1 dropdown must be open at any time
  useEffect(() => {
    // if (openSortDropdown) setOpenFilterDropdown(false);
    if (openFilterDropdown && openSortDropdown) setOpenFilterDropdown(false);
  }, [openSortDropdown]);

  useEffect(() => {
    if (openSortDropdown && openFilterDropdown) setOpenSortDropdown(false);
    // if (openFilterDropdown) setOpenSortDropdown(false);
  }, [openFilterDropdown]);

  return (
    <div className="w-full">
      <SearchBar
        setSearchInput={setSearchInput}
        loading={searchLoading}
        label="Search"
      />
      <div className="w-full my-2">
        <div className="flex justify-center">
          <div className="flex w-10/12">
            <Dropdown
              label="Filters"
              openDropdown={openFilterDropdown}
              setOpenDropdown={setOpenFilterDropdown}
            >
              <FilterMenu filter={filter} setFilter={setFilter} />
            </Dropdown>
            <Dropdown
              label="Sort"
              openDropdown={openSortDropdown}
              setOpenDropdown={setOpenSortDropdown}
            >
              <SortMenu sort={sort} setSort={setSort} />
            </Dropdown>
          </div>
        </div>
      </div>
    </div>
  );
};

export default SearchSection;
