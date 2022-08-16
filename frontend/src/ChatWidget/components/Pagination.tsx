import React, {
  FunctionComponent,
  MouseEventHandler,
  useEffect,
  useState,
} from "react";

interface PaginationProps {
  setCurrPage: React.Dispatch<React.SetStateAction<number>>;
  currPage: number;
  totalElements: number;
  pageSize: number;
}

interface PaginationBtnProps {
  children: React.ReactNode;
  onClick: MouseEventHandler;
  borderless?: boolean;
  active?: boolean;
}

const MAX_INT = 2147483647;
const FORWARD = 0;
const BACKWARD = 1;
const MAX_DISPLAY = 3;

const BackIcon = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      className="h-3 w-3 lg:h-4 lg:w-4"
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
      strokeWidth={2}
    >
      <path strokeLinecap="round" strokeLinejoin="round" d="M15 19l-7-7 7-7" />
    </svg>
  );
};

const FrontIcon = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      className="h-3 w-3 lg:h-4 lg:w-4"
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
      strokeWidth={2}
    >
      <path strokeLinecap="round" strokeLinejoin="round" d="M9 5l7 7-7 7" />
    </svg>
  );
};

const PaginationBtn: FunctionComponent<PaginationBtnProps> = ({
  children,
  onClick,
  borderless,
  active,
}) => {
  return (
    <button
      onClick={onClick}
      className={`${
        active ? "bg-slate-500 text-white" : "bg-transparent text-slate-700"
      } font-semibold text-xs py-1 px-2 lg:text-base lg:py-2 lg:px-4 ${
        !borderless ? "border border-slate-500" : "border-0"
      } rounded`}
    >
      {children}
    </button>
  );
};

const Pagination: FunctionComponent<PaginationProps> = ({
  setCurrPage,
  currPage,
  totalElements,
  pageSize,
}) => {
  const [totalPages, setTotalPages] = useState<number>(
    Math.ceil(totalElements / pageSize)
  );
  const [displayedPages, setDisplayedPages] = useState<number[]>(
    [...Array(MAX_DISPLAY)].fill(MAX_INT)
  );

  // reevaluate displayed pages
  useEffect(() => {
    const newDisplayedPages: number[] = [...Array(MAX_DISPLAY)].fill(MAX_INT);

    [...Array(MAX_DISPLAY)].forEach((e, i) => {
      if (i < totalPages) newDisplayedPages[i] = i + 1;
    });
    setDisplayedPages(newDisplayedPages);
  }, [totalPages]);

  //reevaluate total pages
  useEffect(() => {
    setTotalPages(Math.ceil(totalElements / pageSize));
  }, [totalElements, pageSize]);

  /**
   * Function that handles front / back shifting when the ... button is clicked
   * @param direction direction of shift
   */
  const handleShift = (direction: number) => {
    const currDisplayedPages = [...displayedPages];

    currDisplayedPages.forEach(
      (e, i) => (currDisplayedPages[i] = direction === FORWARD ? e + 1 : e - 1)
    );
    setDisplayedPages(currDisplayedPages);
    if (direction === FORWARD)
      setCurrPage(currDisplayedPages[currDisplayedPages.length - 1]);
    else setCurrPage(currDisplayedPages[0]);
  };

  return (
    <div
      className="
		w-full
		flex
		"
    >
      <div
        className="
			w-full
			flex
			justify-center
			"
      >
        <div>
          {/* Backward button */}
          <PaginationBtn
            borderless
            onClick={() => {
              {
                if (currPage == 1) return;
                if (currPage === displayedPages[0])
                  return handleShift(BACKWARD);
                setCurrPage(currPage - 1);
              }
            }}
          >
            {" "}
            <BackIcon />{" "}
          </PaginationBtn>

          {/* Page numbers */}
          {displayedPages[0] != MAX_INT && displayedPages[0] > 1 ? (
            <PaginationBtn borderless onClick={() => handleShift(BACKWARD)}>
              {" "}
              ...
            </PaginationBtn>
          ) : null}
          {displayedPages.map((e) => {
            if (e != MAX_INT)
              return (
                <PaginationBtn
                  key={e}
                  onClick={() => setCurrPage(e)}
                  active={currPage === e}
                >
                  {e}
                </PaginationBtn>
              );
            return null;
          })}
          {displayedPages[displayedPages.length - 1] < totalPages ? (
            <PaginationBtn borderless onClick={() => handleShift(FORWARD)}>
              ...
            </PaginationBtn>
          ) : null}

          {/* Forward button */}
          <PaginationBtn
            borderless
            onClick={() => {
              {
                if (currPage == totalPages) return;
                if (currPage === displayedPages[displayedPages.length - 1])
                  return handleShift(FORWARD);
                setCurrPage(currPage + 1);
              }
            }}
          >
            {" "}
            <FrontIcon />{" "}
          </PaginationBtn>
        </div>
      </div>
    </div>
  );
};

export default Pagination;
