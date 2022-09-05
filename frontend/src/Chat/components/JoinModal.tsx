import React, { FunctionComponent, useState } from "react";
import { OUTGOING_JOIN } from "../../constants";
import { useAuth } from "../../context/authContext";

const GroupIcon: FunctionComponent = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={1.5}
      stroke="currentColor"
      className="mx-auto mb-4 text-gray-400 w-14 h-14 dark:text-gray-200"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="M18 18.72a9.094 9.094 0 003.741-.479 3 3 0 00-4.682-2.72m.94 3.198l.001.031c0 .225-.012.447-.037.666A11.944 11.944 0 0112 21c-2.17 0-4.207-.576-5.963-1.584A6.062 6.062 0 016 18.719m12 0a5.971 5.971 0 00-.941-3.197m0 0A5.995 5.995 0 0012 12.75a5.995 5.995 0 00-5.058 2.772m0 0a3 3 0 00-4.681 2.72 8.986 8.986 0 003.74.477m.94-3.197a5.971 5.971 0 00-.94 3.197M15 6.75a3 3 0 11-6 0 3 3 0 016 0zm6 3a2.25 2.25 0 11-4.5 0 2.25 2.25 0 014.5 0zm-13.5 0a2.25 2.25 0 11-4.5 0 2.25 2.25 0 014.5 0z"
      />
    </svg>
  );
};

interface JoinModalProps {
  setOpenJoinRoomModal: React.Dispatch<React.SetStateAction<boolean>>;
}

const JoinModal: FunctionComponent<JoinModalProps> = ({
  setOpenJoinRoomModal,
}) => {
  const [roomIdInput, setRoomIdInput] = useState<string>("");
  const [passwordInput, setPasswordInput] = useState<string>("");
  const auth = useAuth();

  return (
    <div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
      <div className="relative w-full p-4 flex justify-center item-center">
        <div className="relative bg-white rounded-lg shadow dark:bg-gray-700">
          <button
            onClick={() => {
              setOpenJoinRoomModal(false);
            }}
            type="button"
            className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white"
            data-modal-toggle="popup-modal"
          >
            <svg
              className="w-5 h-5"
              fill="currentColor"
              viewBox="0 0 20 20"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              ></path>
            </svg>
          </button>
          <div className="p-6 text-center">
            <GroupIcon />
            <h3 className="mb-5 text-lg font-normal text-gray-500 dark:text-gray-400">
              Join a new room
              {/* <span className="font-medium">{currRemovingUser?.username}</span>? */}
            </h3>

            {/* Inputs*/}
            <input
              type="number"
              onKeyDown={(evt) => ["e", "E", "+", "-"].includes(evt.key) && evt.preventDefault()}
              value={roomIdInput}
              onChange={(e) => setRoomIdInput(e.target.value)}
              className="mb-2 bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-gray-500 focus:border-gray-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-gray-500 dark:focus:border-gray-500"
              placeholder="Room id/number"
            />

            <input
              type="password"
              value={passwordInput}
              onChange={(e) => setPasswordInput(e.target.value)}
              className="mb-2 bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-gray-500 focus:border-gray-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-gray-500 dark:focus:border-gray-500"
              placeholder="Room password (Optional)"
            />

            <button
              onClick={() => {
                // set loading state to true

                // emit ws join room event
                auth?.chatSocket?.emit(
                  OUTGOING_JOIN,
                  JSON.stringify({
                    roomId: parseInt(roomIdInput, 10),
                    password: passwordInput,
                  })
                );

                // close window
                setOpenJoinRoomModal(false);
              }}
              type="button"
              className={`w-full text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              Join room
            </button>
            <button
              onClick={() => {
                // if (loading) return;
                setOpenJoinRoomModal(false);
              }}
              type="button"
              className={`w-full mt-2 text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default JoinModal;
