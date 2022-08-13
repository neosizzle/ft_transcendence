import React, { FunctionComponent, useEffect, useState } from "react";
import { NavigateFunction, useNavigate } from "react-router-dom";
import { API_ROOT } from "../../../constants";
import { User } from "../../../context/authContext";
import useWindowDimensions from "../../../hooks/useWindowDimensions";
import { auth_net_get } from "../../../utils";
import Alert, { AlertType } from "../../common/Alert";
import CardLoader from "../../common/CardLoader";
import Pagination from "../../common/Pagination";
import ActionModal from "./components/ActionModal";
import SearchSection from "./components/SearchSection";
import UserCard from "./components/UserCard";

const PAGE_SIZE = 5;
const MOBILE_WIDTH = 426;

export interface Filter {
  username?: string;
  intraName?: string;
  email?: string;
  status?: string;
  [key: string]: string | undefined;
}

export interface Sort {
  sortOn: string;
  sortBy: string;
  [key: string]: string;
}

// fetches users and sets user state
const fetchUsers = async (
  reqUrl: string,
  navigate: NavigateFunction,
  setTotalElements: React.Dispatch<React.SetStateAction<number>>,
  setUsers: React.Dispatch<React.SetStateAction<User[] | null>>
) => {
  const data = await auth_net_get(reqUrl);

  if (data.error && data.error === "Forbidden") navigate("/logout");
  setTotalElements(data.total_elements);
  setUsers(data.data);
};

const Search: FunctionComponent = () => {
  const [filter, setFilter] = useState<Filter>({ username: "" }); // current filter value
  const [sort, setSort] = useState<Sort>({
    sortBy: "Ascending",
    sortOn: "username",
  }); // current sort value
  const [searchInput, setSearchInput] = useState<string>(""); // search input
  const [searchLoading, setSearchLoading] = useState<boolean>(false); //search loading state
  const [currPage, setCurrPage] = useState<number>(1); // curr page of data
  const [totalElements, setTotalElements] = useState<number>(0); // total number of elements
  const [reqUrl, setReqUrl] = useState<string>(
    `${API_ROOT}/users?page=${currPage}&pageSize=${PAGE_SIZE}`
  ); // request url
  const [users, setUsers] = useState<User[] | null>(null); // userlist
  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  }); //open alert box
  const [alertMsg, setAlertMsg] = useState<string>(""); // alert message
  const [currSelectedUser, setCurrSelectedUser] = useState<User | null>(null); // selected user for mobile
  const navigate = useNavigate();
  const dimensions = useWindowDimensions();

  // change req url when filter / search / sort / pagination changes
  useEffect(() => {
    // populate filter with search input
    const filterPopulated = { ...filter };
    Object.keys(filterPopulated).forEach((key) => {
      if (key !== "status") {
        if (typeof key === "string") filterPopulated[key] = searchInput;
      }
    });

    // generate new req url
    let newReqUrl = `${API_ROOT}/users?page=${currPage}&pageSize=${PAGE_SIZE}`;

    // append filterons
    if (Object.keys(filterPopulated).length > 0) newReqUrl += "&filterOn=";
    Object.keys(filterPopulated).forEach((key, i) => {
      if (i != Object.keys(filterPopulated).length - 1) newReqUrl += key + ",";
      else newReqUrl += key;
    });

    // append filterbys
    if (Object.keys(filterPopulated).length > 0) newReqUrl += "&filterBy=";
    Object.keys(filterPopulated).forEach((key, i) => {
      if (i != Object.keys(filterPopulated).length - 1)
        newReqUrl += filterPopulated[key] + ",";
      else newReqUrl += filterPopulated[key];
    });

    // append sorts
    if (Object.keys(sort).length > 0)
      newReqUrl += `&sortOn=${sort["sortOn"]}&sortBy=${sort["sortBy"]}`;

    // send request
    setReqUrl(newReqUrl);
  }, [filter, sort, searchInput, currPage]);

  // everytime req url changes, fetch data
  useEffect(() => {
    // set loading state into true
    setSearchLoading(true);

    // fetch users
    fetchUsers(reqUrl, navigate, setTotalElements, setUsers);

    // set loading state into false
    setSearchLoading(false);
  }, [reqUrl]);

  return (
    <div
      onClick={(e) => {
        // close all action menus (svg clicked)
        if (typeof (e.target as Element).className !== "string") {
          const className: SVGAnimatedString = (e.target as Element)
            .className as unknown as SVGAnimatedString;
          if (currSelectedUser && className.baseVal.includes("menu-toggle"))
            setCurrSelectedUser(null);
          return;
        }

        // close all action menus
        if (
          currSelectedUser &&
          !(e.target as Element).className.includes("action-menu")
        )
          setCurrSelectedUser(null);
      }}
    >
      <div className="flex flex-col items-center pb-20 sm:pb-0 overflow-auto">
        {/* Searchbar and dropdown */}
        <SearchSection
          searchLoading={searchLoading}
          filter={filter}
          setFilter={setFilter}
          sort={sort}
          setSort={setSort}
          setSearchInput={setSearchInput}
        />

        {!users ? (
          <CardLoader />
        ) : users.length > 0 ? (
          <div className="w-full">
            {/* user list */}
            <div>
              {users?.map((user, i) => {
                return (
                  <UserCard
                    user={user}
                    key={i}
                    setCurrSelectedUser={setCurrSelectedUser}
                    currSelectedUser={currSelectedUser}
                    setOpenAlert={setOpenAlert}
                    setAlertMsg={setAlertMsg}
                  />
                );
              })}
            </div>

            {/* Pagination */}
            <Pagination
              setCurrPage={setCurrPage}
              currPage={currPage}
              totalElements={totalElements}
              pageSize={PAGE_SIZE}
            />
          </div>
        ) : (
          <div>No results</div>
        )}

        {/* ActionMenu */}
        {currSelectedUser &&
        dimensions.width &&
        dimensions.width <= MOBILE_WIDTH ? (
          <ActionModal
            user={currSelectedUser}
            setAlertMsg={setAlertMsg}
            setOpenAlert={setOpenAlert}
            setCurrUser={setCurrSelectedUser}
          />
        ) : null}

        {/* Alert */}
        {openAlert.isOpen ? (
          <Alert
            alert={openAlert}
            setOpenAlert={setOpenAlert}
            message={alertMsg}
          />
        ) : null}
      </div>
    </div>
  );
};

export default Search;
