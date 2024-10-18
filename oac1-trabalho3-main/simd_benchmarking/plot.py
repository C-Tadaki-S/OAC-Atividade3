from __future__ import annotations # Suport python 3.7 e 3.8

import json
from pathlib import Path
from dataclasses import dataclass
import subprocess
import re
import sys
import shutil

import matplotlib.pyplot as plt


from loguru import logger
# Configure logger
logger.remove()
logger.add(sys.stderr, format="<level>{level: <5}</level> | <level>{message}</level>", level="DEBUG")

plots_folder = Path("plots")

@dataclass
class PerfResult:
    counter_value: float | None
    event: str
    metric_unit: str
    metric_value: float
    variance: float
    unit: str

    @staticmethod
    def from_dict(d: dict):
        return PerfResult(
            counter_value=d['counter_value'],
            event=d['event'],
            metric_unit=d['metric_unit'],
            metric_value=float(d['metric_value']),
            variance=float(d['variance']),
            unit=d['unit'],
        )

    @staticmethod
    def parse(line: bytes):
        [counter_value, unit, event, variance, event_runtime, pcnt_running, metric_value, metric_unit] = line.split(';')

        if counter_value == "<not counted>":
            logger.warning(f'value not counted: {line}')

        return PerfResult(
            counter_value=float(counter_value.strip()) if counter_value != "<not counted>" else None,
            event=event,
            metric_unit=metric_unit,
            metric_value=float(metric_value.strip().replace(',','.') or 0),
                variance=float(variance.strip().replace('%', '').replace(',','.')),
            unit=unit,
        )

    def __str__(self) -> str:
        counter_value = "" if self.counter_value is None else self.counter_value
        return f"{counter_value:>12} {self.unit:>4} {self.event:>20} # {self.metric_value:>3.2f} ± {self.variance:>3.2f}% {self.metric_unit}"

def load_csv(path: Path):
    with path.open() as f:
        [cycles, instr, duration] = [PerfResult.parse(l.strip()) for l in f]

    return {
        "origin": path.stem,
        "cycles": cycles.counter_value,
        "instr": instr.counter_value,
        "duration": duration.counter_value / 1e9, # ns to s
        "instr_per_cycle": instr.metric_value,
    }

def main():
    # Load load_csvs
    edge_float = load_csv(Path("edge_float.csv"))
    edge_simd = load_csv(Path("edge_simd.csv"))

    # Create plots folder
    plots_folder.mkdir(exist_ok=True)

    # Plot duration
    fig, ax = plt.subplots()
    ax.set_title("Duration")
    ax.set_ylabel("Duration (s)")
    ax.set_xlabel("Implementation")
    ax.bar(
        ["Float", "SIMD"],
        [edge_float['duration'], edge_simd['duration']],
    )
    fig.savefig(plots_folder / "duration.png")

    # Plot IPC
    fig, ax = plt.subplots()
    ax.set_title("Instructions per cycle")
    ax.set_ylabel("Instructions per cycle")
    ax.set_xlabel("Implementation")
    ax.bar(
        ["Float", "SIMD"],
        [edge_float['instr_per_cycle'], edge_simd['instr_per_cycle']],
    )
    fig.savefig(plots_folder / "ipc.png")

# Run main only if this file is called directly
if __name__ == "__main__":
    main()
