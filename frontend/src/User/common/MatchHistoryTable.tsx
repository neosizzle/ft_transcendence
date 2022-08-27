import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { User } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import { Match } from "../types";
import Pagination from "./Pagination";

const PAGE_SIZE = 10;
const games_endpoint = `${API_ROOT}/matches`;

interface TableRowProps {
  match: Match;
  user: User;
}

const TableRow: FunctionComponent<TableRowProps> = ({ match, user }) => {
  return (
    <tr>
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
        {new Date(match.createdAt).toLocaleString()}
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
        {user.id === match.playerId0
          ? match.player1.username
          : match.player0.username}
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
        {match.isCustom ? "Custom" : "Classic"}
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
        {`${match.playerScore0} : ${match.playerScore1}`}
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
        {match.winner.username}
      </td>
    </tr>
  );
};

interface MatchHistoryTableProps {
  user: User;
}
const MatchHistoryTable: FunctionComponent<MatchHistoryTableProps> = ({
  user,
}) => {
  const [matches, setMatches] = useState<Match[] | null>(null);
  const [currPage, setCurrPage] = useState<number>(1);
  const [totalElements, setTotalElements] = useState<number>(-1);
  const [loading, setLoading] = useState<boolean>(false);
  const navigate = useNavigate();

  // get matches info everytime page changes
  useEffect(() => {
    setLoading(true);
    auth_net_get(
      `${games_endpoint}?page=${currPage}&pageSize=${PAGE_SIZE}&operator=OR&filterOn=playerId0,playerId1&filterBy=${user.id},${user.id}`
    ).then((data) => {
      if (data.error && data.error == "Forbidden") return navigate("/logout");
      setMatches(data.data);
      setTotalElements(data.total_elements);
      setLoading(false);
    });
  }, [currPage]);

  return (
    <div className="flex justify-center pt-4">
      <div className="w-5/6">
        <div
          className="
				pb-1
				text-lg
				"
        >
          <h1>Match history</h1>
        </div>
        {/* <!-- ====== Table Section Start --> */}
        <section className=" pb-20 sm:pb-0">
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
                      </tr>
                    </thead>
                    <tbody>
                      {loading ? (
                        <tr>
                          <td>Loading lah</td>
                        </tr>
                      ) : matches && matches.length === 0 ? (
                        <tr>
                          <td>u have to much life di, go play some game</td>
                        </tr>
                      ) : (
                        matches?.map((match) => (
                          <TableRow user={user} match={match} key={match.id} />
                        ))
                      )}
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          </div>
          <Pagination
            setCurrPage={setCurrPage}
            currPage={currPage}
            pageSize={PAGE_SIZE}
            totalElements={totalElements}
          />
        </section>
        {/* <!-- ====== Table Section End --> */}
      </div>
    </div>
  );
};

export default MatchHistoryTable;
